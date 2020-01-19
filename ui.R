library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(janitor)

diet_carbon <- readr::read_csv(here::here("data", "diet_carbon_emission.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(type = ifelse(type == "vegan", "Vegan", type),
         type = ifelse(type == "No beef", "Chicken, pork, sea food", type),
         type = ifelse(type == "Meat lover", "Beef, lamb", type)) 

transport_gram_per_mile <- read_csv(here::here("data", "transport_gram_per_mile.csv"))



shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("GREEN FOOTPRINT"),
    navbarPage(
      "Calculate Your Daily Carbon Footprint since today!",
      
      tabPanel("Calculator",
               mainPanel(
                 tabsetPanel(
                   tabPanel("Diet",
                            sidebarPanel(
                              selectInput(inputId = "diet_type1",
                                          label = "Breakfast type",
                                          choices = c("-- Choose --", unique(diet_carbon$type))
                              ),
                              selectInput(inputId = "diet_type2",
                                            label = "Lunch type",
                                            choices = c("-- Choose --", unique(diet_carbon$type))
                              ),
                              selectInput(inputId = "diet_type3",
                                              label = "Dinner type",
                                              choices = c("-- Choose --", unique(diet_carbon$type))
                              ),
                              actionButton("action1", "Calculate", class = "btn-primary")
                            ),
                            
                            tableOutput(outputId = "diet_table"),
                            
                            p("Your total amount of carbon produced (kilograms) through daily diet is:"),
                            textOutput(outputId = "diet_carbon_total")
                            ),
                   
                   tabPanel("Transportation", 
                            sidebarPanel(
                              selectInput("trans_select_1", 
                                          label = "Transportation type 1 ", 
                                          choices = unique(transport_gram_per_mile$trans_type)
                              ),
                              numericInput("distance_1", 
                                           label = "Distance 1 (miles)", 
                                           value = 0
                              ),
                              selectInput("trans_select_2", 
                                          label = "Transportation type 2", 
                                          choices = unique(transport_gram_per_mile$trans_type)
                              ),
                              numericInput("distance_2", 
                                           label = "Distance 2 (miles)", 
                                           value = 0
                              ),
                              selectInput("trans_select_3", 
                                          label = "Transportation type 3", 
                                          choices = unique(transport_gram_per_mile$trans_type)
                              ),
                              numericInput("distance_3", 
                                           label = "Distance 3 (miles)", 
                                           value = 0
                              ),
                              actionButton("action2", "Calculate", class = "btn-primary")
                            
                            ),
                            
                            mainPanel(
                              p("Transportation carbon emission:"),
                              tableOutput(outputId = "trans_carbon"),
                              p("Your total amount of carbon produced (grams) through transport is:"),
                              textOutput(outputId = "trans_carbon_total")
                            )),
                   
                   tabPanel("Summary", 
                            "This panel is intentionally left blank")
                  )
                )
      ),
      
      tabPanel("Data Sources", 
               "This panel is intentionally left blank"),
      
      tabPanel("Team", 
               "This panel is intentionally left blank")
    )
  )
)


# Define UI for miles per gallon application
# shinyUI(fluidPage(theme = "bootstrap.css",
#                   titlePanel("here is my title"),
#                   sidebarLayout(
#                     sidebarPanel("my widgets are here",
#                                  selectInput(inputId = "diet_type",
#                                              label = "Choose the type of your meal:",
#                                              choices = unique(diet_carbon$type)
#                                              )
#                                   ),
#                     mainPanel("My outputs are here!",
#                               tableOutput(outputId = "candy_table"))
#                                 )
#         ))


#               
#   fileInput("file", "File input:"),
#   textInput("txt", "Text input:", "general"),
#   sliderInput("slider", "Slider input:", 1, 100, 30),
#   tags$h5("Deafult actionButton:"),
#   actionButton("action", "Search"),
#   
#   tags$h5("actionButton with CSS class:"),
#   actionButton("action2", "Action button", class = "btn-primary")
# ),


# tabsetPanel(
#   tabPanel("Tab 1",
#            h4("Table"),
#            tableOutput("table"),
#            h4("Verbatim text output"),
#            verbatimTextOutput("txtout"),
#            h1("Header 1"),
#            h2("Header 2"),
#            h3("Header 3"),
#            h4("Header 4"),
#            h5("Header 5")
#   )
# ,
# tabPanel("Tab 2", "This panel is intentionally left blank"),
# tabPanel("Tab 3", "This panel is intentionally left blank")
#)