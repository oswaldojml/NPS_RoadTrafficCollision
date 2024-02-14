# importing libraries
library(stats19)
library(tidyverse)
library(lubridate)
library(skimr)

setwd("Documents/Nonparametric Statisics/Project/raw data")

#crashes

full_collisioin_data <- readr::read_csv(file ="dft-road-casualty-statistics-collision-1979-latest-published-year.csv") 

spec(full_collisioin_data)
glimpse(full_collisioin_data)

full_collisions <- format_collisions(full_collisioin_data)

spec(full_collisions)
glimpse(full_collisions)

# for this we have 8.8 milion collisions, we select the years that we want 

# lets select the years:
# we focus on the last 18 from 2005 to 2022 to avoid problems in the labels of the vehicles
# we thus have 216 months or 936 weeks in the case of funtional data

full_collisions <- full_collisions %>% filter(accident_year > 2004) 
full_collisioin_data <- full_collisioin_data %>% filter(accident_year > 2004)

# we have 2.5 milion crashes 

#turning into factors categorical variables:

to_change <- c(8,9,13,15,18,20,22,23,24,26,27,28,29,30,31,32,33,34,35)
full_collisions[,to_change] <- lapply(full_collisions[,to_change] , factor)

spec(full_collisions)
glimpse(full_collisions)

# fixing the problem with the road numbers:

full_collisions$first_road_number <- full_collisioin_data$first_road_number
full_collisions$second_road_number <- full_collisioin_data$second_road_number

# ordering the days:

full_collisions$day_of_week <- factor(full_collisions$day_of_week, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

skim(full_collisions)

# we have 300 crashes for which the location is missing, nothing i can do to recover it

# we have some missing local_authority_district, but we have the 
# local_authority_ons_district so we should use that instead.

# we have 37 missing speed limits, this are unclassified roads such as driveways, nothing i can do about it

# we have some missing second_road_class, putting it as unclassified would be wrong since here seems to
# be junctions at the accident site

# we have 28 row where datetime failed but we have the date and the time so we should fix it

na_index <- which(is.na(full_collisions$datetime))

dt <- as.POSIXct(paste(full_collisions[na_index,]$date, format(full_collisions[na_index,]$time, "%H:%M:%S")),tz = "GMT")

full_collisions[na_index,"datetime"] <- dt

skim(full_collisions)

# ok this is fixed

# we can save this 
save(full_collisions,file = "full_collisions.RData")


# removing the location information

collisions_no_location <- full_collisions %>% 
  select(accident_index,accident_year,accident_reference,police_force,accident_severity,number_of_vehicles,
         number_of_casualties,date,day_of_week,time,first_road_class,road_type,speed_limit,junction_detail,
         junction_control,second_road_class,pedestrian_crossing_human_control,pedestrian_crossing_physical_facilities,
         light_conditions,weather_conditions,road_surface_conditions,special_conditions_at_site,carriageway_hazards,
         did_police_officer_attend_scene_of_accident,trunk_road_flag,datetime)

# saving it 

save(collisions_no_location,file = "collisions_no_location.RData")

# casualties

full_casualties_data <- readr::read_csv(file ="dft-road-casualty-statistics-casualty-1979-latest-published-year.csv") 

spec(full_casualties_data)
glimpse(full_casualties_data)

# filtering the years
full_casualties_data <- full_casualties_data %>% filter(accident_year > 2004)

full_casualties <- format_casualties(full_casualties_data)
# 4 milion casualtues

spec(full_casualties)
glimpse(full_casualties)

skimr::skim(full_casualties)

# we can remove the age since most are missing since introduced in 2016

full_casualties <- full_casualties %>% select(!c(age_of_casualty,lsoa_of_casualty,casualty_imd_decile,
                                                 pedestrian_road_maintenance_worker))

#turning into factors categorical variables:

to_change <- c(6:15)
full_casualties[,to_change] <- lapply(full_casualties[,to_change] , factor)

glimpse(full_casualties)

skim(full_casualties)

# we have 36 missing casualty types, only high level description of the casualty

# saving it

save(full_casualties,file = "full_casualties.RData")

# vehicles

full_vehicles_data <- readr::read_csv(file ="dft-road-casualty-statistics-vehicle-1979-latest-published-year.csv") 

spec(full_vehicles_data)
glimpse(full_vehicles_data)

# filtering the years
full_vehicles_data <- full_vehicles_data %>% filter(accident_year > 2004)
glimpse(full_vehicles_data)

full_vehicles <- format_vehicles(full_vehicles_data)
#4.7 milion vehicles

glimpse(full_vehicles)

skimr::skim(full_vehicles)

# we can remove generic_make_model since only from 2020
# same for age_of_driver and engine capacity

full_vehicles <- full_vehicles %>% select(!c(age_of_driver,generic_make_model,engine_capacity_cc,
                                             driver_imd_decile,lsoa_of_driver))

#turning into factors categorical variables:

names(full_vehicles)
to_change <- c(5:21,23)
full_vehicles[,to_change] <- lapply(full_vehicles[,to_change] , factor)

# ordering the days:

full_vehicles$day_of_week <- factor(full_vehicles$day_of_week, 
    c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


glimpse(full_vehicles)

skim(full_vehicles)

# we only have some missing vehicle_direction_from and vehicle_direction_to, probably useless for us

save(full_vehicles,file = "full_vehicles.RData")

#let's build the sequences of total crashes in different time horizons: 
#year, month, week, day

#years:
df_dayly <- df %>% group_by(date) %>% summarise(n = n()) %>% 
  mutate(year = year(date),month = month(date),day = day(date),
         day_of_year = yday(date),weekday = wday(date),week_of_year = isoweek(date))

df_year <- df_dayly %>% group_by(year,day_of_year) %>% summarise(total_crashes = sum(n)) %>% 
  pivot_wider(names_from = day_of_year, values_from = total_crashes, values_fill = 0) %>% ungroup()

#months:

df_month <- df_dayly %>% group_by(year,month,day) %>% summarise(total_crashes = sum(n)) %>% 
  pivot_wider(names_from = day, values_from = total_crashes, values_fill = 0) %>% ungroup()

#weeks:

df_week <- df_dayly %>% group_by(year,week_of_year,weekday) %>% summarise(total_crashes = sum(n)) %>% 
  pivot_wider(names_from = weekday, values_from = total_crashes, values_fill = 0) %>% ungroup()

# here we have weeks that are cut off at the beginning and end of each year!! 

# hours:

df_hour <- df %>% select(date,time) %>% mutate(hour = hour(time)) %>% group_by(date,hour) %>% 
  summarize(tot_crashes = n()) %>% pivot_wider(names_from = hour, values_from = tot_crashes,
                                               values_fill = 0) %>% ungroup()

#saving the datasets:

save(df_year,file = "df_year.RData")
save(df_month,file = "df_month.RData")
save(df_week,file = "df_week.RData")
save(df_hour,file = "df_hour.RData")





