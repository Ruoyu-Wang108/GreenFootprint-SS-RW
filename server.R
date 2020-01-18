library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(janitor)

# Define server logic required to plot various variables against mpg
shinyServer <- function(input, output) {
  
  diet_emission <- reactive({
    diet_carbon %>%
      filter(type == input$diet_type) %>%
      select(type, per_meal_kg)
  })
  
  output$diet_table <- renderTable({
    diet_emission()
  })

}