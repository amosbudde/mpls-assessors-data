
library(tidyverse)

## parcel data and mpls2040 data from https://opendata.minneapolismn.gov/
rd_parcels <- read.csv("/Users/amosbudde/Downloads/Assessors_Parcel_Data_2020.csv",colClasses=c("APN"="character"))
rd_mpls2040 <- read.csv("/Users/amosbudde/Downloads/Future_Land_Use_and_Built_Form_2040.csv",colClasses=c("PID"="character"))

## merge mpls2040 map on parcel data. add in the definition of non-conforming parcels.
## include only urban neighborhood land use, interior1,2,3 built form
mpls_data <- merge(x=rd_mpls2040, y=rd_parcels, by.x = "PID", by.y = "APN", all = TRUE) %>%
  select(PID, FORMATTED_ADDRESS, ZIPCODE, NEIGHBORHOOD, WARD, LANDUSE, Land_Use, Built_Form, 
         ZONING, ABOVEGROUNDAREA, PARCEL_AREA_SQFT, TOTAL_UNITS, PROPERTY_TYPE, 
         LANDVALUE, BUILDINGVALUE, TOTALVALUE, BEDROOMS, ABOVEGROUNDAREA
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


mpls_data %>%
  filter (Built_Form == 'Interior 1', TOTAL_UNITS == 3, nonconforming_any == 1)

mpls_data %>%
  group_by(TOTAL_UNITS, Built_Form) %>%
  summarize(n(), mean(FAR), mean(noncomforming_far), mean(nonconforming_lot_size), mean(nonconforming_any))


## share of lots nonconforming by WARD. 
## THERE IS AN ANALYSIS HERE TO RUN. How many I1/2/3 lots are illegal to build? Where are they distributed? 
#Tweet #1. 14k. 
# There are 75,482 1-3 unit homes in Minneapolis. 
# 18% of them are nonconforming because they are on a lot size lower than the minimum. 
# 4% are nonconforming because their FAR is too large. 
# 20% overall couldn't be built today. 
# But because existing housing stock is over 89% SFHs, this is not quite fair. Actually it's 50% of triplexes, 30% of duplexes, etc. So these rules make it harder. 
# This also shows how much a 0.5 FAR cap does not affect our SFH stock, but it does impact our 2-3plex housing stock. 
# And this is looking at current data. So though Minneapolis allowed 3plexes, the city's interior neighborhoods are not getting any denser than the status quo. 

mpls_data %>%

  mutate(FAR_OVER050 = case_when(FAR > 0.5 ~ 1 , TRUE ~ 0)) %>%
  mutate(illegal_to_build = case_when(is_nonconforming == 1 | FAR > 0.5 ~ 1 , TRUE ~ 0)) %>%
#  group_by(TOTAL_UNITS) %>%
  summarize(n(), mean(is_nonconforming), mean(FAR_OVER050), mean(illegal_to_build))

mpls_data %>%
  filter(substr(Built_Form,1,8) == 'Interior') %>%
  filter(NEIGHBORHOOD == 'UNIVERSITY', FAR > 0.5)
