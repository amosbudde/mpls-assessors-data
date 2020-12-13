
## Dec11 Tweetstorm Non-Conforming I1-3 Homes.R

## This analysis provided data for this thread: https://twitter.com/amosbudde/status/1337499248695230464
## Data output here https://docs.google.com/spreadsheets/d/1JCQg-Vpp4ys9u0iTKhrCnGw7axO98EYwudvS49YCVV8/edit#gid=1038754191

library(tidyverse)
library(googlesheets4)

## parcel data and mpls2040 data from https://opendata.minneapolismn.gov/
rd_parcels <- read.csv("/Users/amosbudde/Downloads/Assessors_Parcel_Data_2020.csv",colClasses=c("APN"="character"))
rd_mpls2040 <- read.csv("/Users/amosbudde/Downloads/Future_Land_Use_and_Built_Form_2040.csv",colClasses=c("PID"="character"))

## merge mpls2040 map on parcel data. add in the definition of non-conforming parcels.
## include only urban neighborhood land use, interior1,2,3 built form
mpls_data <- merge(x=rd_mpls2040, y=rd_parcels, by.x = "PID", by.y = "APN", all = TRUE) %>%
  select(PID, FORMATTED_ADDRESS, ZIPCODE, NEIGHBORHOOD, WARD, LANDUSE, Land_Use, Built_Form, 
         ZONING, ABOVEGROUNDAREA, PARCEL_AREA_SQFT, TOTAL_UNITS, PROPERTY_TYPE, 
         TOTALVALUE, BEDROOMS, ABOVEGROUNDAREA
         ) %>% 
  filter(
    !is.na(PID),                          #remove records without a parcel ID - these are just a few odd cases
    !is.na(PARCEL_AREA_SQFT),             #remove records without parcel area
    PARCEL_AREA_SQFT > 800,               #800 sqft is currently the smallest lot size with housing in Minneapolis 
    substr(Built_Form,1,8) == 'Interior', #I1-3 Districts
    TOTAL_UNITS %in% c(1,2,3)             #1-3plexes only
  ) %>%
  mutate(
    nonconforming_lot_size = case_when(
      ZONING %in% c('R1', 'R2') &  PARCEL_AREA_SQFT < 6000 ~ 1, 
      ZONING %in% c('R1', 'R2') &  PARCEL_AREA_SQFT >= 6000 ~ 0, 
      PARCEL_AREA_SQFT < 5000 ~ 1 ,
      PARCEL_AREA_SQFT >= 5000 ~ 0 
      ) ,
    FAR = ABOVEGROUNDAREA / PARCEL_AREA_SQFT , 
    noncomforming_far = case_when(
      Built_Form %in% c('Interior 1', 'Interior 2') & ABOVEGROUNDAREA / PARCEL_AREA_SQFT >  0.5 ~ 1 , 
      Built_Form %in% c('Interior 1', 'Interior 2') & ABOVEGROUNDAREA / PARCEL_AREA_SQFT <= 0.5 ~ 0 , 
      Built_Form == 'Interior 3' & TOTAL_UNITS == 1 & ABOVEGROUNDAREA / PARCEL_AREA_SQFT >  0.5 ~ 1 , 
      Built_Form == 'Interior 3' & TOTAL_UNITS == 2 & ABOVEGROUNDAREA / PARCEL_AREA_SQFT >  0.6 ~ 1 , 
      Built_Form == 'Interior 3' & TOTAL_UNITS == 3 & ABOVEGROUNDAREA / PARCEL_AREA_SQFT >  0.7 ~ 1 , 
      Built_Form == 'Interior 3' ~ 0
      )
    ) %>%
  mutate(nonconforming_any = pmax(noncomforming_far, nonconforming_lot_size))

### START TWITTER THREAD ###

#1 14,678 (19%) of homes are non-conforming (2524 have FAR to high, 13650 have lot size too small, some have both.). 
chart1 <- mpls_data %>%
  summarize("1-3 Unit Homes in MPLS" = n(), 
            "Non-Conforming: Any reason" = mean(nonconforming_any), 
            "FAR too high" = mean(noncomforming_far), 
            "Lot Size too small" = mean(nonconforming_lot_size)
  )

#2 Break out by # units. 19% of duplexes and 37% of triplexes in Interior Districts. 
chart2 <- mpls_data %>%
  group_by(TOTAL_UNITS) %>%
  summarize("1-3 Unit Homes in MPLS" = n(), 
            "Non-Conforming: Any reason" = mean(nonconforming_any), 
            "FAR too high" = mean(noncomforming_far), 
            "Lot size too small" = mean(nonconforming_lot_size)
  )

#3 By Ward! 
chart3 <- mpls_data %>%
  group_by(WARD) %>%
  summarize("1-3 Unit Homes in MPLS" = n(), 
            "Non-Conforming: Any reason" = mean(nonconforming_any), 
            "FAR too high" = mean(noncomforming_far), 
            "Lot size too small" = mean(nonconforming_lot_size)
  )

#4 By Neighborhood? 
chart4 <- mpls_data %>%
  group_by(NEIGHBORHOOD) %>%
  summarize("1-3 Unit Homes in MPLS" = n(), 
            "Non-Conforming: Any reason" = mean(nonconforming_any), 
            "FAR too high" = mean(noncomforming_far), 
            "Lot size too small" = mean(nonconforming_lot_size)
  )

# make Google sheets
gs4_create("twitter_charts_dec11",  sheets = list(chart1,chart2,chart3,chart4))
