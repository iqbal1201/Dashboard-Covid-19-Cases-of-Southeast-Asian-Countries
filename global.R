# import libs
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(scales)
library(glue)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(leaflet)
library(rgdal)


# import dataset

global_confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
                             check.names = F)

global_death <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                         check.names = F)

global_recover <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                           check.names = F)

# Wrangling dataset
global_confirmed_clean <- global_confirmed %>%
  pivot_longer(-c("Province/State", "Country/Region", Lat, Long),
               names_to = "date",
               values_to = "number_cases") %>%
  rename(province_state = "Province/State",
         country_region = "Country/Region") %>%
  mutate(date = mdy(date)) %>%
  mutate(month = month(date)) %>%
  group_by(country_region, date, Long, Lat) %>%
  summarise(total_confirm = sum(number_cases))


global_death_clean <- global_death %>%
  pivot_longer(-c("Province/State", "Country/Region", Lat, Long),
               names_to = "date",
               values_to = "number_cases") %>%
  rename(province_state = "Province/State",
         country_region = "Country/Region") %>%
  mutate(date = mdy(date)) %>%
  mutate(month = month(date)) %>%
  group_by(country_region, date, Long, Lat) %>%
  summarise(total_death = sum(number_cases))


global_recover_clean <- global_recover %>%
  pivot_longer(-c("Province/State", "Country/Region", Lat, Long),
               names_to = "date",
               values_to = "number_cases") %>%
  rename(province_state = "Province/State",
         country_region = "Country/Region") %>%
  mutate(date = mdy(date)) %>%
  mutate(month = month(date)) %>%
  group_by(country_region, date, Long, Lat) %>%
  summarise(total_recover = sum(number_cases))


#Filtering for Southeast Asian countries
sea_confirmed <- global_confirmed_clean %>%
  filter(country_region %in% c("Indonesia", "Malaysia", "Thailand", "Singapore", "Vietnam",
                               "Cambodia", "Laos", "Burma", "Philippines", "Brunei", "Timor-Leste"))

sea_death <- global_death_clean %>%
  filter(country_region %in% c("Indonesia", "Malaysia", "Thailand", "Singapore", "Vietnam",
                               "Cambodia", "Laos", "Burma", "Philippines", "Brunei", "Timor-Leste"))

sea_recover <- global_recover_clean %>%
  filter(country_region %in% c("Indonesia", "Malaysia", "Thailand", "Singapore", "Vietnam",
                               "Cambodia", "Laos", "Burma", "Philippines", "Brunei", "Timor-Leste"))

#Merge

sea_all_case <- sea_confirmed %>% 
  left_join(sea_recover) %>% 
  left_join(sea_death) 


sea_case_growth <- sea_all_case %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(growth_confirm = total_confirm - lag(total_confirm, default = 0),
         growth_death = total_death - lag(total_death, default = 0),
         growth_recover = total_recover - lag(total_recover, default = 0))

sea_case_growth$country_region[sea_case_growth$country_region == "Burma"] <- "Myanmar" #Changing Burma -> Myanmar


#Theme
theme_algoritma <- theme(legend.key = element_rect(fill="black"),
                         legend.background = element_rect(color="white", fill="#263238"),
                         plot.subtitle = element_text(size=6, color="white"),
                         panel.background = element_rect(fill="#dddddd"),
                         panel.border = element_rect(fill=NA),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_line(color="darkgrey", linetype=2),
                         panel.grid.minor.y = element_blank(),
                         plot.background = element_rect(fill="#263238"),
                         text = element_text(color="white"),
                         axis.text = element_text(color="white")
                         
)
 # Import another dataset contains health and economic information
countries_info <- read.csv("sea_countries_info.csv")

sea_covid_update <- sea_case_growth %>%
  filter(date == max(date))%>%
  mutate(cfr = total_death/total_confirm,
         recovery_rate = total_recover/total_confirm)

sea_covid_update_info <- sea_covid_update %>%
  left_join(countries_info) %>%
  mutate(confirm_per_100000 = total_confirm/100000)


sea_case_growth2 <- sea_case_growth %>%
  left_join(countries_info, by = "country_region")


# Prepare data for map (leaflet)

shape <- raster::shapefile("SEA_countries.shp") #import shapefile of southeast asian (SEA) countries

sea_covid_new <- sea_covid_update %>%  
  rename(NAME = country_region) #changing name for left_join

shape@data <- shape@data %>% dplyr::left_join(sea_covid_new, by = "NAME") #left join dataframe with shapefile



# create a color palette with handmade bins

library(RColorBrewer)
mybins <- c(0, 10000, 50000, 100000, 500000, 1000000)
mypalette <- colorBin(palette="YlOrRd", 
                      domain=shape@data$total_confirm, 
                      na.color="transparent", 
                      bins=mybins)

# Prepare label
mytext <- paste(shape@data$NAME) %>%
  lapply(htmltools::HTML)


#Prepare popup
popup_shape <- paste("<h3><b>", shape@data$NAME, "</b></h3>", 
                     "Total of Cases: ", shape@data$total_confirm, "<br>", 
                     "Total of Recovery: ", shape@data$total_recover, "<br>",
                     "Total of Death: ", shape@data$total_death, "<br>",
                     "Case Fatality Rate: ", round(shape@data$cfr, 2), "<br>",
                     sep="")


