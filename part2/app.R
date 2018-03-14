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

ui <- navbarPage("Veg Analysis",  
                 
                 tabPanel("Visualize",
                            mainPanel(
                              plotOutput("distPlot")
                            )
                          ),
                 tabPanel("Toxicity Measurements",
                          mainPanel(
                            plotOutput("distPlot2")
                          )
                        ),
  
                  tabPanel("Evaluation",
                          mainPanel(
                            plotOutput("distPlot3")
                          )
                        )

)

server <- function(input, output) {
  
}
# Run the application 
shinyApp(ui = ui, server = server)

