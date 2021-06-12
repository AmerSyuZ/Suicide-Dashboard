#Main App
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)
library(glue)
library(leaflet)
library(skimr)
library(countrycode)
library(shiny)
library(shinydashboard)
library(sf)
library(rgeos)
library(ggspatial)
library(rworldmap)
library(shinycssloaders)
library(tidyverse)


#Data preparation

data <- read.csv("master.csv")

suicide <- data %>%
  select(-(HDI.for.year),-(country.year),-(gdp_for_year....),-(gdp_per_capita....),-(generation)) %>%
  setNames(c("country",
             "year",
             "sex",
             "group_age",
             "suicides_numbers",
             "population",
             "suicides_rate")
  ) %>% 
  mutate(country=as.factor(country),
         country_code=countrycode(country,origin="country.name",destination="iso3c"),
         continent=as.factor(countrycode(country,origin="country.name",destination = "continent")),
         year=as.numeric(year),
         sex=as.factor(sex),
        date_year=year(as.Date(as.character(year),format="%Y")),
         group_age=as.factor(group_age)
  ) %>% filter(year!=2016) %>% filter(country!="Dominica") %>% filter(country!="Saint Kitts and Nevis") %>% 
  filter(country!="Macau") %>% filter(country!="Cabo Verde") %>% mutate(tahun=as.factor(year))

continent_worldwide_line <-
  suicide %>%
  group_by(year) %>% 
  summarise(rate = 100000*sum(suicides_numbers)/sum(population)) %>% 
  mutate(continent = "Worldwide") %>% 
  select(continent, everything())

continent_line_data <-
  suicide %>% 
  group_by(continent, year) %>% 
  summarise(rate = 100000*sum(suicides_numbers)/sum(population))%>% 
  ungroup() %>% 
  rbind(continent_worldwide_line) %>% 
  filter(rate != 0) %>% 
  mutate(label = paste0('<b>', continent, '</b>',
                        ' (', year, ')<br>',
                        'Rate: ', round(rate, 2), ' per 100,000'))

