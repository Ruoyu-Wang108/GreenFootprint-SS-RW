library(shiny)
library(tidyverse)
library(kableExtra)
library(shinythemes)
library(here)
library(janitor)


# Define server logic required to plot various variables against mpg
shinyServer <- function(input, output) {
  
  transport_input <- reactive({
    
    data.frame(
      trans_type = c(input$trans_select_1, input$trans_select_2, input$trans_select_3),
      distance = c(input$distance_1, input$distance_2, input$distance_3)
    )
    
  })
  
  transport_carbon <- reactive({
      transport_gram_per_mile %>% 
      filter(trans_type %in% c(input$trans_select_1, input$trans_select_2, input$trans_select_3)) %>% 
      dplyr::full_join(transport_input(), by = "trans_type") %>%
      mutate("Transportation types" = trans_type,
             "Travel distance" = distance,
             "Carbon emission (gram)" = distance*carbon_burden) %>% 
      select(-c(trans_type, distance, carbon_burden))
  })
  
  
  output$trans_carbon <- renderTable({
    transport_carbon()
  })
  
  
  output$trans_carbon_total <- renderText({
    
    sum(transport_carbon()$"Carbon emission (gram)")
    
  })
 
  
  
  
# Diet interact table  
  diet_input <- reactive({
    
    data.frame(
      meal = c("Breakfast", "Lunch", "Dinner"),
      type = c(input$diet_type1, input$diet_type2, input$diet_type3)
    )
    
  })

  
  diet_emission <- reactive({
    diet_carbon %>% 
      filter(type %in% c(input$diet_type1, input$diet_type2, input$diet_type3)) %>% 
      dplyr::full_join(diet_input(), by = "type") %>%
      mutate("Meal" = meal,
             "Type" = type,
             "Carbon emission (kilogram)" = per_meal_kg) %>% 
      select(-(type:meal))
  })
  

  output$diet_table <- renderTable({
    diet_emission()
  })
  
  output$diet_carbon_total <- renderText({
    
    sum(diet_emission()$"Carbon emission (kilogram)", na.rm = TRUE)
    
  })

}

