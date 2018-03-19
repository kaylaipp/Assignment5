#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
require(gridExtra)
library(zoo)
library(shinydashboard)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(title = "NOAA Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Temperature Analysis", tabName = "airtemp"),
      menuItem("Water Temperature Analysis", tabName = "watertemp"),
      menuItem("Comparison", tabName = "compare"),
      menuItem("Temperature Change", tabName = "climate"),
      menuItem("Effects of our data selection", tabName = "reading")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "airtemp",
              h2("Air Temperature Analysis"),
              p("First, a table of the statistics:"),
              fluidRow(
                box(tableOutput('table1'),width=7)
              ),
              p("As we'll show you later, it's not coincidence the highest temperature is
                from 2014, a recent year."),
              fluidRow(
                box(plotOutput("plot1", width = "100%", height = "300px"), width=12)
              ),
              p("The above is a time series represenation of data collected for air temperatures
                from NOAA's National Data Buoy Center at buoy 46035 located in the Bering Sea,
                except for the years 2012 and 2013, for which data was missing, which was
                collected from the nearby buoy 46070.")
      ),
      
      tabItem(tabName = "watertemp",
              h2("Water Temperature Analysis"),
              p("First, a table of the statistics:"),
              fluidRow(
                box(tableOutput('table2'),width=7)
              ),
              p("As we'll show you later, it's not coincidence the highest temperature is
                from 2016."),
              fluidRow(
                box(plotOutput("plot2", width = "100%", height = "300px"), width=12)
              ),
              p("The above is a time series represenation of data collected for water temperatures
                from NOAA's National Data Buoy Center at buoy 46035 located in the Bering Sea,
                except for the years 2012 and 2013, for which data was missing, which was
                collected from the nearby buoy 46070.")
      ),
      
      tabItem(tabName = "compare",
              h2("Comparison of Air Temperature and Water Temperature"),
              fluidRow(
                box(htmlOutput("comparison", inline=TRUE), width=12)
              )
      ),
      
      tabItem(tabName = "climate",
              h2("Climate Change Analysis"),
              fluidRow(
                box(htmlOutput("tempchange", inline=TRUE), width=12)
              )
      ),
      
      tabItem(tabName = "reading",
              h2("How does our data affect results?"),
              fluidRow(
                box(htmlOutput("datachoice", inline=TRUE), width=12)
              )
      )
    )
    
    
  )
)

server <- function(input, output) {
  
  NOAA <- read.csv("data.csv")
  #NOAA_big <- read.csv("big_data.csv")
  
  yrs <- 1987:2016
  # Compute statistic tables
  median <- aggregate(na.omit(NOAA$ATMP), list(year(NOAA$date[complete.cases(NOAA$ATMP)])), median)$x
  median[length(median)+1] <- median(na.omit(NOAA$ATMP))
  mean <- aggregate(na.omit(NOAA$ATMP), list(year(NOAA$date[complete.cases(NOAA$ATMP)])), mean)$x
  mean[length(mean)+1] <- mean(na.omit(NOAA$ATMP))
  std <- aggregate(na.omit(NOAA$ATMP), list(year(NOAA$date[complete.cases(NOAA$ATMP)])), sd)$x
  std[length(std)+1] <- sd(na.omit(NOAA$ATMP))
  min <- aggregate(na.omit(NOAA$ATMP), list(year(NOAA$date[complete.cases(NOAA$ATMP)])), min)$x
  min[length(min)+1] <- min(na.omit(NOAA$ATMP))
  max <- aggregate(na.omit(NOAA$ATMP), list(year(NOAA$date[complete.cases(NOAA$ATMP)])), max)$x
  max[length(max)+1] <- max(na.omit(NOAA$ATMP))
  
  air_stats <- data.frame(median, mean, std, min, max)
  row.names(air_stats) <- c(yrs, 'Total')
  
  median <- aggregate(na.omit(NOAA$WTMP), list(year(NOAA$date[complete.cases(NOAA$WTMP)])), median)$x
  median[length(median)+1] <- median(na.omit(NOAA$WTMP))
  mean <- aggregate(na.omit(NOAA$WTMP), list(year(NOAA$date[complete.cases(NOAA$WTMP)])), mean)$x
  mean[length(mean)+1] <- mean(na.omit(NOAA$WTMP))
  std <- aggregate(na.omit(NOAA$WTMP), list(year(NOAA$date[complete.cases(NOAA$WTMP)])), sd)$x
  std[length(std)+1] <- sd(na.omit(NOAA$WTMP))
  min <- aggregate(na.omit(NOAA$WTMP), list(year(NOAA$date[complete.cases(NOAA$WTMP)])), min)$x
  min[length(min)+1] <- min(na.omit(NOAA$WTMP))
  max <- aggregate(na.omit(NOAA$WTMP), list(year(NOAA$date[complete.cases(NOAA$WTMP)])), max)$x
  max[length(max)+1] <- max(na.omit(NOAA$WTMP))
  
  water_stats <- data.frame(median, mean, std, min, max)
  row.names(water_stats) <- c(yrs, 'Total')
  

  correlation <- cor(NOAA$ATMP, NOAA$WTMP, use = "pairwise.complete.obs")
  ttest_air <- t.test(NOAA$ATMP[year(NOAA$date) == 1988], NOAA$ATMP[year(NOAA$date) == 2016], paired=TRUE)
  ttest_water <- t.test(NOAA$WTMP[year(NOAA$date) == 1988], NOAA$WTMP[year(NOAA$date) == 2016], paired=TRUE)
  
  
  
  #Takes too long
  #ttest_2 <- t.test(NOAA_big$ATMP[year(NOAA_big$date) == 1988], NOAA_big$ATMP[year(NOAA_big$date) == 2016][-1], paired=TRUE)$statistic
  
  #Time Series #1: Air Temp 
  output$plot1 <- renderPlot({
    dataTimeSeries <- ts(na.approx(NOAA$ATMP), start = c(1987,1),frequency = 365.25)
    plot(dataTimeSeries, xlab = "Time", ylab="Air Temperature (Celsius)")
  })
  
  output$table1 <- renderTable({air_stats}, rownames=TRUE)
  output$table2 <- renderTable({water_stats}, rownames=TRUE)
  
  #Time Series #2: Water Temp
  output$plot2 <- renderPlot({
    dataTimeSeries <- ts(na.approx(NOAA$WTMP), start = c(1987,1), frequency=365.25)
    plot.ts(dataTimeSeries, xlab = "Time", ylab="Water Temperature (Celsius)")
  })
  
  #Comparison
  output$comparison <- renderText({
    paste("It would seem that there should be a relation between air temperature and sea
          temperature. 
          <br> <br>
          To confirm this, we found the correlation coefficient for the two
          datasets. The correlation coefficient for air and water temperature is ", correlation,
          ". 
          <br><br>
          This implies that there is a strong positive correlation between the two,
          which is exactly what we'd expect.")
  })
  
  output$tempchange <- renderText({
    paste("Has the mean temperature of water and air in the Bering sea changed over the past
          30 years?",
          "<br><br>", "We performed two tailed t-tests on the data from 1988 and 2016 to help find the 
          answer to this question.The alternative hypothesis in this case is that 
          the true difference in means is not equal to 0.", 
          "<br><br>", "For air temperature, the t-value is ", ttest_air$statistic, 
          " and the p-value is ", ttest_air$p.value, ". <br> For water
          temperature, the t-value is ", ttest_water$statistic, " and the p-value is ", 
          ttest_water$p.value, ".",
          "<br><br>", "These values imply that it is extrremely likely that 
          temperatures have changed over the past 30 years, confirming the results of 
          most scientists.")
  })
  
  output$datachoice <- renderText({
    paste("In our analysis, we use only one sample per day day out of 24 daily 
          hourly temperature readings, specifically at noon. <br>
          Though it's more computationally expensive to find the data, using 
          the full range of data gives much more conclusive results when doing
          the t-test. <br><br>
          For air temperature, we get the much more conclusive t-value of -40.728. <br>
          For water temperature, we similarly see a value t-value of -194.67, which is
          nearly indisputable evidence. <br> <br>
          However, since our results were already fairly conclusive, this seems
          unnecessary. Thus we believe our results to still be worth consideration.
          <br><br>
          In addition, it's worth nothing that adding more data merely decreases the
          variance. However, there will always be some variance, so the impact of
          this is not all that significant.")
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

