library(shiny)
library(tidyverse)
library(kableExtra)
library(shinythemes)
library(here)
library(janitor)

library(shinyPagerUI)

global_co2 <-  read_csv(here::here("data", "global_co2_2014.csv"))


diet_carbon <- readr::read_csv(here::here("data", "diet_carbon_emission.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(type = ifelse(type == "vegan", "Vegan", type),
         type = ifelse(type == "No beef", "Chicken, pork, sea food", type),
         type = ifelse(type == "Meat lover", "Beef, lamb", type)) 

transport_gram_per_mile <- read_csv(here::here("data", "transport_gram_per_mile.csv"))