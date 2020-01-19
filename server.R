library(shiny)
library(tidyverse)
library(kableExtra)
library(shinythemes)
library(here)
library(janitor)


global_co2 <-  read_csv(here("data","global_co2_2014.csv"))

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
      carbon = c(round(sum(diet_emission()$"Carbon emission (kg)", na.rm = TRUE), 2),
                 round(sum(transport_carbon()$"Carbon emission (kg)"), 2))
    )
    
  })
  
# Text summary
  output$carbon_total <- renderText({
    
    round(sum(diet_trans_data()$carbon), 2)
    
  })
  
  
  output$diet_trans <- renderPlot({
    
   ggplot(diet_trans_data(), 
          aes(x = type, y = carbon, fill = type)) +
      geom_col(alpha = 0.75, 
               width = 0.5,
               show.legend = FALSE) +
      theme_minimal() +
      labs(x = "",
           y = "Daily carbon emission (kg)") +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
  })
  

# Total emission in global setting
     
  total_co2 <- reactive({
    sum(transport_carbon()$"Carbon emission (kg)")/0.33 + sum(diet_emission()$"Carbon emission (kg)"/0.33, na.rm = TRUE) 
  })
  
  
  output$global_co2 <- renderPlot({
    
    ggplot(data = global_co2, aes(x = daily_2014)) +
      geom_histogram(aes(y=..density..), bins = 50, fill = "white") + 
      geom_vline(xintercept = total_co2(), color = "green") +
      annotate("text",
               x = total_co2(), 
                    y = .8, 
                    label = "Here's your today's carbon emission",
                    color = "grey45",
                    fontface =2)+
      scale_y_continuous(limits = c(0, 0.02)) +
      stat_function(fun = dnorm, args = list(mean = mean(global_co2$daily_2014), sd = sd(global_co2$daily_2014))) +
      theme_minimal()+
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank()) +
      labs(x = "Daily carbon emissions (kg)",
           y = "Global frequency")
    
  })
  
  
  output$suggestion <- renderText({
    
    "You can always reduce your carbon emission by choosing a vegan/vegetarian diet or consuming less beef or lamb.
    You can walk, bike or taking public transit to work or school. 
    If you need a car, you can save two thirds of your CO2 emission by choosing a hybrid over a regular gasoline car. 
    An electric car can further reduce your emission by half.
    If you are already optimizing your carbon emissions, CHEERS! You ARE the winner and leader!
    For more detailed information, please refer to the Data Sources section!"
    
  })
  
  
  # Data Source
  
  output$source_intro <- renderText({
    
    "Diet data is derived from research done by Shrink That Footprint.
               Car emission data is computed from the dataset provided by Fuel Ecomony.
               Global distribution of individual CO2 emission is derived from the per capita CO2 emission of 264 countries
               scaled by each country's population in 2014 downloaded from The World Bank. We assume transportation and diet 
    together account for 33% of each individual's carbon emission based on a personal blog calculate the daily carbon emission."
    
  })
  
  output$diet_original_data <- renderTable({
    
    diet_carbon %>% 
      select(type, annual_amount_tons, per_day_emission_g, per_meal_kg) %>% 
      mutate("Diet types" = type,
             "Annual (kg)" = annual_amount_tons*1000,
             "Daily (kg)" = per_day_emission_g/1000,
             "Each meal (kg)" = per_meal_kg
             ) %>% 
      select("Diet types", "Annual (kg)", "Daily (kg)", "Each meal (kg)")
    
  })
  
  output$transport_original_data <- renderTable({
    
    transport_gram_per_mile %>% 
      filter(trans_type != "No transportation") %>% 
      rename("Transportation types" = trans_type) %>% 
      mutate("CO2 emissions (kg/mile)" = carbon_burden/1000) %>% 
      select(-carbon_burden) 
      
  })
  
  output$diet_link <- renderText({
    "http://shrinkthatfootprint.com/food-carbon-footprint-diet"
  })
  
  output$car_link <- renderText({
    "https://www.fueleconomy.gov/feg/download.shtml"
  })
  
  output$not_car_link <- renderText({
    "https://www.ourstreetsmpls.org/does_bike_commuting_affect_your_carbon_footprint_and_how_much"
    
  })
  
  output$bus_link <- renderText({
    "https://www.citylab.com/transportation/2012/11/can-we-please-stop-pretending-cars-are-greener-transit/3960/"
  })
  
  
  output$worldbank_link <- renderText({
    "https://data.worldbank.org/indicator/en.atm.CO2e.pc"
  })
  
  output$worldbank_pop_link <- renderText({
    "https://data.worldbank.org/indicator/SP.POP.TOTL"
  })
  
  output$scale <- renderText({
    "https://sites.psu.edu/mfsblog/2015/02/04/how-much-co2-do-we-produce/"
  })
  
  # Team
  
  output$our_team <- renderText({
    
    "We are students in the Bren School of Environmental Science and Management 
              at the University of California, Santa Barbara. We both believe that 
               spreading sustainable lifestyles is one of the most important things we need to do in the 21st century. 
               We want to raise people's awareness about their environmental impacts generated through the most basic daily activities.
               This carbon accounting project for individual users is just a beginning."
    
  })
  
  output$thank_note <- renderText({
    
    "Thank you!!!"
    
  })

}

