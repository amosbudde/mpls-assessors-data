
## Dec13 Tweet Assessed Value by Building Use.R

## This analysis provided data for this thread: https://twitter.com/amosbudde/status/1337499248695230464
## Data output here https://docs.google.com/spreadsheets/d/1JCQg-Vpp4ys9u0iTKhrCnGw7axO98EYwudvS49YCVV8/edit#gid=1038754191


library(tidyverse)
library(sqldf)
rd_parcels <- read.csv("/Users/amosbudde/Downloads/Assessors_Parcel_Data_2020.csv",colClasses=c("APN"="character")) 

names(rd_parcels)

rd_parcels %>% 
  mutate(value_per_sqft = round(TOTALVALUE / PARCEL_AREA_SQFT,3), acres = PARCEL_AREA_SQFT/43560) %>%
  filter(PARCEL_AREA_SQFT > 0, TOTALVALUE > 0, ! grepl('CONDO',BUILDINGUSE), ! grepl('CO-OP',BUILDINGUSE)) %>%
  group_by(BUILDINGUSE) %>%
  summarize( n = n(), total_sqft = sum(PARCEL_AREA_SQFT), total_acres = sum(acres), assessed_value = sum(TOTALVALUE), value_per_sqft = mean(value_per_sqft)) %>%
  arrange(desc(value_per_sqft)
          
          dat1 <- rd_parcels %>% 
            mutate(
              value_per_sqft = round(TOTALVALUE / PARCEL_AREA_SQFT,5), 
              acres = PARCEL_AREA_SQFT/43560
              , ZIP = substr(ZIPCODE,1,5)
            ) %>%
            filter(PARCEL_AREA_SQFT > 0, TOTALVALUE > 0, ! grepl('CONDO',BUILDINGUSE), ! grepl('CO-OP',BUILDINGUSE), ! is.na(BUILDINGUSE)) %>%
            group_by(ZIP) %>%
            mutate(nzip=n(), avg_value_sqft_zip = mean(value_per_sqft)) %>%
            ungroup() %>%
            mutate(value_per_sqft_diff_from_avg = value_per_sqft - avg_value_sqft_zip) %>%
            select(BUILDINGUSE, FORMATTED_ADDRESS, PARCEL_AREA_SQFT, TOTALVALUE, value_per_sqft, acres, ZIP, nzip, avg_value_sqft_zip, value_per_sqft_diff_from_avg)
          
          datout <- dat1 %>%
            group_by(BUILDINGUSE) %>%
            summarize( n = n(), 
                       total_sqft = sum(PARCEL_AREA_SQFT), 
                       total_acres = sum(acres), 
                       assessed_value = sum(TOTALVALUE), 
                       value_per_sqft = mean(value_per_sqft),
                       avg_value_sqft_zip = mean(avg_value_sqft_zip), 
                       value_per_sqft_diff_from_avg = mean(value_per_sqft_diff_from_avg)
            ) %>%
            arrange(value_per_sqft_diff_from_avg)
          