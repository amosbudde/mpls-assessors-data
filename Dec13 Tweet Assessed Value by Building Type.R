
## Dec13 Tweet Assessed Value by Building Use.R

## This analysis provided data for this thread: https://twitter.com/amosbudde/status/1338176220123418624
## Data output here: https://docs.google.com/spreadsheets/d/1IEFjYva6mnA-8A2ZSxigHSDN34k9LzKgJmAI9T-9lZU/edit#gid=583800848

library(tidyverse)
library(googlesheets4)

rd_parcels <- read.csv("/Users/amosbudde/Downloads/Assessors_Parcel_Data_2020.csv",colClasses=c("APN"="character")) 

dat1 <- rd_parcels %>% 
  filter(PARCEL_AREA_SQFT > 0, TOTALVALUE > 0, ! grepl('CONDO',BUILDINGUSE), ! grepl('CO-OP',BUILDINGUSE)) %>%
  mutate(
    value_per_sqft = round(TOTALVALUE / PARCEL_AREA_SQFT,5),
    acres = PARCEL_AREA_SQFT/43560
    , ZIP = substr(ZIPCODE,1,5)
    ) %>%
  group_by(ZIP) %>%
  mutate(n_zip=n(), avg_value_sqft_zip = mean(value_per_sqft)) %>%
  ungroup() %>%
  mutate(value_per_sqft_diff_from_zip = value_per_sqft - avg_value_sqft_zip) %>%
  select(BUILDINGUSE, FORMATTED_ADDRESS, PARCEL_AREA_SQFT, LANDUSE, TOTALVALUE, LANDVALUE, value_per_sqft, acres, ZIP, n_zip, avg_value_sqft_zip, value_per_sqft_diff_from_zip) %>%
  filter(substr(ZIP,1,2)=='55', n_zip > 100)

dat2 <- dat1 %>%
  group_by (BUILDINGUSE) %>%
  summarize(n_building_use = n(), 
            acres = sum(acres), 
            sqft = sum(PARCEL_AREA_SQFT), 
            value_per_sqft_zip = mean(avg_value_sqft_zip), 
            value_per_sqft = mean(value_per_sqft), 
            value_per_sqft_diff_from_zip = mean(value_per_sqft_diff_from_zip)
            ) %>%
  arrange(value_per_sqft)

gs4_create("twitter_charts_dec13",  sheets = list(dat2))
