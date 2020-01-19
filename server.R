library(shiny)
library(tidyverse)
library(kableExtra)
library(shinythemes)
library(here)
library(janitor)


global_co2 <-  read_csv(here("data" ,"global_co2_2014.csv"))

# Define server logic required to plot various variables against mpg
shinyServer <- function(input, output) {
  
  transport_input <- eventReactive( input$action2,{
    
    data.frame(
      trans_type = c(input$trans_select_1, input$trans_select_2, input$trans_select_3),
      distance = c(input$distance_1, input$distance_2, input$distance_3)
    )
    
  })
  
  transport_carbon <- eventReactive( input$action2, {
      transport_gram_per_mile %>% 
      filter(trans_type %in% c(input$trans_select_1, input$trans_select_2, input$trans_select_3)) %>% 
      dplyr::full_join(transport_input(), by = "trans_type") %>%
      mutate("Transportation types" = trans_type,
             "Travel distance" = distance,
             "Carbon emission (kg)" = distance*carbon_burden/1000) %>% 
      select(-c(trans_type, distance, carbon_burden))
  })
  
  
  output$trans_carbon <- renderTable({
    transport_carbon()
  })
  
  
  output$trans_carbon_total <- renderText({
    
    round(sum(transport_carbon()$"Carbon emission (kg)"), 2)
    
  })
 
  
  
  
# Diet interact table  
  diet_input <- reactive({
    
    data.frame(
      meal = c("Breakfast", "Lunch", "Dinner"),
      type = c(input$diet_type1, input$diet_type2, input$diet_type3)
    )
    
  })

  
  diet_emission <- eventReactive( input$action1, {
    diet_carbon %>% 
      filter(type %in% c(input$diet_type1, input$diet_type2, input$diet_type3)) %>% 
      dplyr::full_join(diet_input(), by = "type") %>%
      mutate("Meal" = meal,
             "Type" = type,
             "Carbon emission (kg)" = per_meal_kg) %>% 
      select(-(type:meal))
  })
  

  output$diet_table <- renderTable({
    diet_emission()
  })
  
  output$diet_carbon_total <- renderText({
    
    round(sum(diet_emission()$"Carbon emission (kg)", na.rm = TRUE), 2)
    
  })

# Bar chart
  
  diet_trans_data <- reactive({
    
    data.frame(
      type = c("Diet", "Transportation"),
      carbon = c(round(sum(transport_carbon()$"Carbon emission (kg)"), 2), 
                 round(sum(diet_emission()$"Carbon emission (kg)", na.rm = TRUE), 2))
    )
    
  })
  
  
  output$diet_trans <- renderPlot({
    
   ggplot(diet_trans_data(), 
          aes(x = type, y = carbon, fill = type)) +
      geom_col(alpha = 0.75, 
               width = 0.5,
               show.legend = FALSE) +
      theme_minimal() +
      labs(x = "",
           y = "Carbon emission (kg)",
           fill = "") +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
  })
  

# Total emission in global setting
  
  total_co2 <- reactive({
    sum(transport_carbon()$"Carbon emission (kg)")/0.33 + sum(diet_emission()$"Carbon emission (kg)", na.rm = TRUE) 
  })

  
  output$global_co2 <- renderPlot({
    
    ggplot(data = global_co2_2014, aes(x = daily_2014)) +
      geom_histogram(aes(y=..density..), bins = 50, fill = "white") + 
      geom_vline(xintercept = total_co2(), color = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(global_co2_2014$daily_2014), sd = sd(global_co2_2014$daily_2014))) +
      theme_minimal()+
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank()) +
      labs(x = "Daily carbon emissions (kg)",
           y = "Global frequency")
    
  })
  
}

