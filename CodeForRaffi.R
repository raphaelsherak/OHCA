# For Raffi

library(tidyverse)
library(rgdal)
library(sf)
library(tigris)

## DATA 
data1 <- read.csv("Matched_geocoded_xy.csv") %>% 
  dplyr::select(Incident__, Date_of_Arrest, Location_Type,
                Arrest_Witness_Status,Age, Age_Modifier, Gender, 
                Race_Ethnicity, Etiology=Presumed_Cardiac_Arrest_Etiology, 
                Initiated_CPR, Type_of_Bystander_CPR_Provided, 
                AED_applied=Was_an_AED_applied_prior_to_EMS_arrival,
                x_text, y_text) 

data2 <- read.csv("Tied_geocoded_xy.csv") %>% 
  mutate(Incident_Zip_Code = Fixed_Zip_text) %>% 
  dplyr::select(Incident__, Date_of_Arrest, Location_Type,
                Arrest_Witness_Status, Age, Age_Modifier, Gender, 
                Race_Ethnicity, Etiology=Presumed_Cardiac_Arrest_Etiolog,
                Initiated_CPR, Type_of_Bystander_CPR_Provided, 
                AED_applied=Was_an_AED_applied_prior_to_EMS,
                x_text, y_text) %>%
  mutate(Date_of_Arrest = substr(Date_of_Arrest, 1, 
                                 nchar(Date_of_Arrest)-5))

dt.pre <- rbind(data1, data2) %>%
  mutate(
    Initiated_bin = case_when(
      Initiated_CPR == "Bystander" | 
        Initiated_CPR == "Healthcare Provider (non-911 Responder)" |
        Initiated_CPR == "Family Member" ~ "Bystander", 
      Initiated_CPR == "EMS Responder (transport EMS)" | 
        Initiated_CPR == "First Responder" ~ "Responder"), 
    Etiology_Binary = ifelse(Etiology == "Presumed Cardiac Etiology", 
                             "Cardiac", "Other"),
    Age_Years = ifelse(Age_Modifier == "month", Age/12, Age), 
    Race_Clean = case_when(
      Race_Ethnicity == "Black/African-American" ~ "Black", 
      Race_Ethnicity == "Hispanic/Latino" ~ "Latino", 
      Race_Ethnicity == "White" ~ "White", 
      TRUE ~ "Other"),
    Race_Binary = ifelse(Race_Ethnicity == "White", "White", "Non-White"), 
    Date = parse_date(Date_of_Arrest, "%m/%d/%y")) %>%
  add_count(Incident__) %>%
  # EXCLUSIONS
  filter(Location_Type != "Healthcare Facility" & 
           Location_Type != "Nursing Home", 
         # Remove Not Applicable for now, while waiting for EMS to investigate
         Initiated_CPR != "Not Applicable", 
         # Adults only 
         Age_Years >= 18, 
         n == 1, # remove any duplicate incident IDs
         Gender != "Male-to-Female, Transgender Female",
         Gender != "", 
         Date >= "2022-01-01", # 2022 onward
         y_text < 41.5)

# convert OHCA data to spatial points
points.sp <- SpatialPointsDataFrame(
  data.frame(dt.pre[c("x_text","y_text")]), 
  data = dplyr::select(dt.pre, Incident__),
  proj4string=CRS(as.character("+proj=longlat +datum=NAD83 +no_defs")), 
  bbox = NULL) 

# load in census data
ctcensus <- tracts("CT") %>% 
  filter(COUNTYFP == "170", NAME != "9900")

# what census tracts are our points in? 
points.sp@data$CENSUS <- sp::over(points.sp, as_Spatial(ctcensus))$NAME

## FINAL DATA 
dt <- left_join(points.sp@data, dt.pre) %>% 
  mutate(YEAR = year(Date), 
         age_10 = Age/10, 
         Bystander_bin = ifelse(Initiated_bin == "Responder", 0, 1)) %>%
  filter(!is.na(CENSUS)) 

# filter census data so it only includes tracts with OHCA 
incl.tract <- filter(ctcensus, NAME %in% dt$CENSUS) 



