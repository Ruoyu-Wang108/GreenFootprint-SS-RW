library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(janitor)

diet_carbon <- readr::read_csv(here::here("data", "diet_carbon_emission.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(type = ifelse(type == "vegan", "Vegan", type)) %>% 
  filter(type %in% c("Vegan", "Vegetarian", "Omnivorous average"))

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

shinyUI(
  fluidPage(
    theme = "flatly",
    titlePanel("GREEN FOOTPRINT"),
    
    navbarPage(
      theme = "flatly", id = "navbarColor02", # <--- To use a theme, uncomment this
      "CALCULATE YOUR DAILY CARBON FOOTPRINT!",
      tabPanel("CACULATOR",
               sidebarPanel(
                 fileInput("file", "File input:"),
                 textInput("txt", "Text input:", "general"),
                 sliderInput("slider", "Slider input:", 1, 100, 30),
                 tags$h5("Deafult actionButton:"),
                 actionButton("action", "Search"),
                 
                 tags$h5("actionButton with CSS class:"),
                 actionButton("action2", "Action button", class = "btn-primary")
               ),
               mainPanel(
                 "MY OUTPUTS WILL BE HERE",
               )
      ),
      tabPanel("DATA SOURCES", "This panel is intentionally left blank",
               ),
      tabPanel("TEAM", "This panel is intentionally left blank")
    )
  )
)



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