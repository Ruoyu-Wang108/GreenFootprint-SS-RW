library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(janitor)

diet_carbon <- readr::read_csv(here::here("data", "diet_carbon_emission.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(type = ifelse(type == "vegan", "Vegan", type))

# Define UI for miles per gallon application
shinyUI(fluidPage(theme = "bootstrap.css",
                  titlePanel("here is my title"),
                  sidebarLayout(
                    sidebarPanel("my widgets are here",
                                 selectInput(inputId = "diet_type",
                                             label = "Choose the type of your meal:",
                                             choices = unique(diet_carbon$type)
                                             )
                                  ),
                    mainPanel("My outputs are here!",
                              tableOutput(outputId = "candy_table"))
                                )
        ))