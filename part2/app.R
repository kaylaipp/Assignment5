library(shiny)
library(dplyr)
require(gridExtra)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(shinydashboard)
library(DT)

#didn't include PARAQUAT & ABAMECATIN & DICHLOROPROPENE & EMAMECTIN-BENZOATE & FENPROPATHRIN 
#OXYDEMETON-METHYL, DIMETHOATE, THIODICARB,'METHAMIDOPHOS','DIFLUBENZURON' b/c values were dissolved (D)
#the following list is only chemicals that were not dissolved. 
chemicals <- c('BETA-CYFLUTHRIN','BIFENTHRIN','CHLORANTRANILIPROLE','CHLORPYRIFOS','CYFLUTHRIN',
               'ESFENVALERATE','IMIDACLOPRID','LAMBDA-CYHALOTHRIN','METHOMYL','NALED',
               'PERMETHRIN','THIAMETHOXAM','ZETA-CYPERMETHRIN',
               'DIAZINON','DISULFOTON','CYPERMETHRIN','PRONAMIDE','GAMMA-CYHALOTHRIN')

toxicityLevels <- c(8350,70,5000,270,1271,458,450,79,48,281,4000,1563,
                    250,400,12.5,250,8350,79)
veggies <-c('BROCCOLI', 'CAULIFLOWER')

#turn off warnings & scientific notation for now
options(warn=-1)
options(scipen = 999)
options(shiny.sanitize.errors = FALSE)

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Part 2: Chemical Toxicity", tabName = "dashboard", icon = icon("dashboard")))),
  dashboardBody(
    h1("Toxicity Levels Analysis"),
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                title = "Time Series", status = "info",
                plotOutput("plot1", height = 250)),
              
              box(
                title = "Time Series Controls", status = "success",
                sliderInput("years", "Years: (Note: error message means chemical was not applied in one of specified years)",
                            min = 2006, max = 2016, value = c(2006,2016),sep = ""),
                
                selectInput("chemical", "Chemical:",chemicals), 
                selectInput("veggies1", "Vegetables:",c('BROCCOLI','CAULIFLOWER'))
              )),
            fluidRow(
              box(
                title = "Average Toxicity Levels By Year", status = "info",
                plotOutput("plot2")
              ),
              
              box(
                title = "Bar Plot Controls", status = "danger",
                selectInput("years2", "Year:", c(2006,2010,2014,2016)),
                selectInput("veggies2", "Vegetable:", c('BROCCOLI', 'CAULIFLOWER'))
              )), 
            fluidRow(
              box(
                title = "T-Test Controls", status = "danger",
                selectInput("yeartt", "Year:", c(2006,2010,2014,2016),selected = 2010),
                selectInput("yeartt2", "Year:", c(2006,2010,2014,2016), selected=2014), 
                selectInput("chemicals3", "Chemicals:", chemicals)
              ), 
              box(
                title = "T-Test", status = "success",
                htmlOutput("ttest")
              )
            )
            
    ), 
    fluidRow(
      box(
        title = "Analysis",background = "maroon",
        HTML("<ul><li>Lethal dose (LD50) is the amount of an ingested substance that kills 50 percent of a test sample</li>
             <li>The LD50 values to the right are the oral toxicity levels for a rat. </li>
             <li>The data shows that restricted use chemicals are often applied to household vegetables such as 
             cauliflower and broccoli above the standard LD50 rate</li><li>There is a upward trend of larger doses of pesticides being
             and greater variety of chemicals used in produce as the years go on as seen in the deviating t statistics </li></ul>")
        
        
        ), 
      box(
        title = "Oral Toxicity LD50 Levels (mg/kg)", status = "warning",
        DTOutput('table1'))
      )
    )
    )

#only load data once 
#read in excel data 
veg.1 <- readxl::read_excel("veg1.xlsx")
cnames.1 <- colnames(veg.1)
c <- apply(veg.1, 2, n_distinct)
c[c>1]

d <- names(c[c==1])
e <- names(c[c>1])

veg.2 <- select(veg.1, e)
cnames.2 <- colnames(veg.2)

#Veg.2 is filtered table w/o NA values 
apply(veg.2, 2, n_distinct)

veg.3 <- dplyr::rename(veg.2, 
                       Geo = 'Geo Level', 
                       State = 'State ANSI',
                       Data = 'Data Item',
                       Category = 'Domain Category')

#Veg.3 is w/ shortend column names
cnames.3 <- colnames(veg.3)
yy <- separate(veg.3, Category, into = c("label", "quant"), sep=",")


#ru is table w/ data for only restricted use chemicals 
ru <- filter(yy, label=="RESTRICTED USE CHEMICAL")
ru2 <- filter(yy, label=="RESTRICTED USE CHEMICAL") 

#)
ru2 <- ru2%>%mutate(chem = str_extract(quant, "(?<=\\().*?(?= =)"))
ru3 <- ru2%>%mutate(chem = str_extract(quant, "(?<=\\().*?(?= =)"))
ru4 <- ru2%>%mutate(chem = str_extract(quant, "(?<=\\().*?(?= =)"))
#View(ru4)

#ru1 is table w/ types of diff restricted chemicals
ru1 <- ru %>% select(label, quant) %>% unique()
toxicity <- data.frame(chemicals,toxicityLevels)



server <- function(input, output) {
  #error handling 
  data <- reactive({
    validate(
      need((ru%>%filter(between(Year, input$years[1],input$years[2]), str_detect(quant, input$chemical), Commodity == input$veggies1)), "Please select a data set")
    )
    get(input$data, 'package:datasets')
  })
  
  #Time Series Plot 
  output$plot1 <- renderPlot({
    #filter ru table by year and chemical type 
    print(colnames(ru))
    test <- ru%>%filter(Commodity == input$veggies1)
    #View(test)
    newru <- ru%>%filter(between(Year, input$years[1],input$years[2]), str_detect(quant, input$chemical), Commodity == input$veggies1)
    print(newru$Value)
    
    if(newru$Value == '(D)'){
      newru$Value[newru$Value == '(D)'] <- 0
      print(newru$Value)
      
      maxAmount <- toxicity%>%filter(chemicals == input$chemical)
      Amount <- ts(newru$Value, start = c(input$years[1]), end = input$years[2])
      plot.ts(Amount)
      title(main= paste(input$chemical, " IN ", input$veggies1, " - DISSOLVED (D)"))
      mtext(paste0("Max LD50 value: ", maxAmount$toxicityLevels), side = 3)
    }
    
    else{
      #add ld50 line 
      maxAmount <- toxicity%>%filter(chemicals == input$chemical)
      Amount <- ts(newru$Value, start = c(input$years[1]), end = input$years[2])
      plot.ts(Amount)
      title(main=input$chemical)
      mtext(paste0("Max LD50 value: ", maxAmount$toxicityLevels), side = 3)
      abline(h=maxAmount$toxicityLevels, col="blue")
      legend("topright", "Max LD50 value", col=c("blue"), lty=1, cex=.65)
    }
    
  })
  
  #Barplot 
  output$plot2 <- renderPlot({
    newru2 <- ru2%>%filter(Year == input$years2, Commodity == input$veggies2)
    #replace (D) with 0
    newru2$Value[newru2$Value == '(D)'] <- 0
    newru2$Value[newru2$Value == '(Z)'] <- 0
    
    chems <- newru2$chem
    vals <- newru2$Value
    print(chems)
    df1 = data.frame(chems, vals)
    
    test3 <- newru2 %>% 
      group_by(chem) %>% 
      summarise(mean_val = mean(as.numeric(Value)))
    print(test3)
    par(mar=c(4,11,4,2))
    barplot(test3$mean_val, names.arg = test3$chem, horiz = T, las = 2, col = "blue", xlab = "Amount (mg/kg)")
    title(main=paste(input$veggies2, " PESTICIDES IN " , input$years2))
  })
  
  #TD50 Toxicity Levels Table
  output$table1 = renderDT({
    toxicity <- data.frame(chemicals,toxicityLevels)
    toxicity
  })
  
  
  #T-test
  output$ttest = renderUI({
    y1 <- ru3%>%filter(Year == input$yeartt, chem == input$chemicals3)
    y2 <- ru4%>%filter(Year == input$yeartt2, chem == input$chemicals3)
    
    #replace values with NA so they tables are same length
    y1$Value[y1$Value == '(D)'] <- 0
    y2$Value[y2$Value == '(D)'] <- 0
    
    s1 <- paste0(t.test(as.numeric(y1$Value),as.numeric(y2$Value),paired=TRUE))
    HTML(paste0("<strong>Note:</strong> Error message means specificed chemical was not applied in one of the years selected <br>",
                "Null Hypothesis: There is an increasing of variety and amount of chemicals applied to produce as years progress<br>",
                "Conclusion: There was a significant difference in the paired t statistic and mean when comparing earlier years with more
                recent years. The results suggest that the null hypothesis is true and there is a trend of greater variety of chemicals with
                greater amounts. <br><br>",

                "<ul><li>T statistic: ",s1[1],"</li>",
                "<li>95% confidence interval: ",s1[4],"</li>
                <li>Degrees of freedom: ",s1[2],"</li>
                <li>P-value: ",s1[3],"</li>
                <li>Mean of differences: ",s1[5],"</li><ul>"

    ))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

