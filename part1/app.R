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
library(ggplot2)
library(reshape2)

ui <- navbarPage("NOAA Analysis",  
                 
                 tabPanel("Air Temperature",
                            mainPanel(
                              plotOutput("distPlot")
                            )
                          ),
                 tabPanel("Sea Temperature",
                          mainPanel(
                            plotOutput("distPlot2")
                          )
                        ),
  
                  tabPanel("Comparison",
                          mainPanel(
                            plotOutput("distPlot3")
                          )
                        ),
                 tabPanel("30 Year Change Analysis",
                          mainPanel(
                            plotOutput("distPlot4")
                          )
                        ),
                 tabPanel("One day only effects?",
                          mainPanel(
                            plotOutput("distPlot5")
                          )
                        )

)

server <- function(input, output) {
  
}
# Run the application 
shinyApp(ui = ui, server = server)

