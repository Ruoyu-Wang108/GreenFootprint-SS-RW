library(shiny)
library(tidyverse)
library(kableExtra)
library(shinythemes)
library(here)
library(janitor)


global_co2 <-  read_csv(here("data","global_co2_2014.csv"))

# Define server logic required to plot various variables against mpg
shinyServer <- function(input, output) {
  
  # Introduction
  
  output$overall_intro <- renderText({
    
    "Green Footprint is your personal daily carbon (measured in CO2) emission calculator. Under the Calculator section, begin by choosing your diet types for breakfast,
    lunch, and dinner. Then, enter up to three types of transportation you took and the distance you travelled with them respectively. Finally, you can 
    see your total CO2 emission from diet and transportation for that day, an estimated position of your emission level in the world, and suggestions to live a greener day. "

  })
  
  
  output$overall_intro_spc <- renderText({
  
  "You can choose from vegan, vegetarian, chicken, pork, seefood, beef, lamb, or omnivorous average for types of diet. An 'omnivorous average' means 
  you ate all types of meat for that meal. For transportation, you can choose from walking, biking, taking bus, and driving cars, where cars are divided
  into three categories: gasoline, hybrid, and electric. For other types of public transportations, you may choose bus. If you want to
  leave any choices blank, choose '--Choose--' for Diet and 'No transportation' for Transportation."
  
    
  })
    
  # Transportation carbon emission
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
                    y = 0.1, 
                    label = "Here's your today's carbon emission",
                    color = "grey45",
                    font = 5)+
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
    together account for 33% of each individual's daily carbon emission based on a personal blog calculate the daily carbon emission."
    
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
  
  output$other_link <- renderText({
    "https://www.eomega.org/article/3-biggest-ways-to-reduce-your-environmental-impact"
  })
  
  # Team
  
  output$our_team <- renderText({
    
    "We are students from the Bren School of Environmental Science and Management, University of California, Santa Barbara. In our opinion, sharing the sustainable lifestyles is one of the most wonderful things to do in the 21st century. We want to raise people's awareness that their behaviors matter and might already generate environmental impacts. Hopefully, this carbon accounting project for individual users is just a beginning of our actions."
    
  })
  
  output$thank_note <- renderText({
    
    "We appreciate your attention to our project and the Earth. Thank you!"
    
  })

  output$art_credit <- renderText({
    
    "© Allison Horst, Bren logo artwork. She is a Bren professor. Her design features environmental icons such as a majestic blue whale, wind power turbines, and trees. For more details, click: "
    
  })
  
  

}

