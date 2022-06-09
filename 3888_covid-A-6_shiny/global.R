## Libraries ----
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plotly)
library(dplyr)
library(lubridate)
library(GGally)
# library(randomcoloR)
library(forecast)
library(viridis)
library(dplyr)
library(skimr)

# For url
# library(XML)
# library(RCurl)
# library(rlist)

# For map
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

# Loading data ----

# If you want to update the data, set it as TRUE. 
# Or read csv directly from the link of the data
update_data <- FALSE

if (!dir.exists("data")) {
  dir.create("data")
}

if (update_data | !file.exists("data/owid-covid-data.csv")) {
  download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                destfile = file.path(getwd(), "/data/owid-covid-data.csv"))

}

covid_data <- read.csv("data/owid-covid-data.csv",
                       stringsAsFactors = FALSE,
                       check.names =  FALSE)

covid_data$date <- as.Date(covid_data$date)
covid_data$new_cases[covid_data$new_cases < 0] <- 0
covid_data$new_cases_smoothed[covid_data$new_cases_smoothed < 0] <- 0

covid <- read.csv("data/owid-covid-data.csv")


## MAPS ----

world <- ne_countries(scale = "medium", returnclass = "sf")

developed_countries_full <- filter(covid, human_development_index >= 0.854)
developed_countries_code <- unique(developed_countries_full$iso_code)

world$developed <- ifelse(
  world$iso_a3 %in% developed_countries_code,
  "Yes",
  "No")


## Q1 Ken Part ----

covid_used_ken <- covid[c("location",
                          "date",
                          "new_cases_smoothed",
                          "new_cases_smoothed_per_million",
                          "new_deaths_smoothed",
                          "new_deaths_smoothed_per_million",
                          "population",
                          "population_density")]


covid_A6_ken <- read.csv(
  "data/processed/covid_A6_ken.csv"
)
covid_A6_ken_develop <- read.csv(
  "data/processed/covid_A6_ken_develop.csv"
)



## Q2 Eva Part ----

covid_A6_eva <- read.csv(
  "data/processed/covid_A6_eva.csv"
)

covid_A6_eva_developed <- read.csv(
  "data/processed/covid_A6_eva_developed.csv"
)

covid_A6_eva_developing <- read.csv(
  "data/processed/covid_A6_eva_developing.csv"
)



## Q3 Pat Part ----

covid_A6_pat_developed <- read.csv(
  "data/processed/covid_A6_pat_developed.csv"
)
covid_A6_pat_developing <- read.csv(
  "data/processed/covid_A6_pat_developing.csv"
)




## Q4 Gary Part ----

covid_A6_gary <- read.csv(
  "data/processed/covid_A6_gary.csv"
)



## Q5 Amber Part ----

covid_A6_amber <- read.csv(
  "data/processed/covid_A6_amber.csv"
)

covid_A6_amber_developed <- read.csv(
  "data/processed/covid_A6_amber_developed.csv"
)

covid_A6_amber_developing <- read.csv(
  "data/processed/covid_A6_amber_developing.csv"
)

covid_aus_mal_amber <- rbind(
  covid_A6_amber[covid_A6_amber$location == 'Australia', ],
  covid_A6_amber[covid_A6_amber$location == 'Malaysia', ]
)

Malaysia_amber <- covid_A6_amber[covid_A6_amber$location == 'Malaysia', ]
Malaysia_amber$date <- as.Date(Malaysia_amber$date)

Australia_amber <- covid_A6_amber[covid_A6_amber$location == 'Australia', ]
Australia_amber$date <- as.Date(Australia_amber$date)
