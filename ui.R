library(shiny)
library(shinythemes)

# Define UI for miles per gallon application
shinyUI(fluidPage(theme = "bootstrap.css",
                  pageWithSidebar(
                    
                    # Application title
                    headerPanel("Green Track"),
                    
                    sidebarPanel("Here's the input"),
                    
                    mainPanel("here's the output")
                                  )
                  )
        )