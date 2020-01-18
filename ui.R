library(shiny)
library(shinythemes)

# Define UI for miles per gallon application
shinyUI(fluidPage(theme = "bootstrap.css",
                  pageWithSidebar(
                    
                    # Application title
                    headerPanel("Miles Per Gallon"),
                    
                    sidebarPanel(),
                    
                    mainPanel()
                                  )
                  )
        )