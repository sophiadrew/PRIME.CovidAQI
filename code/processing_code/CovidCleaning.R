###############################
# Processing Script Processing Script #3 - County wide Covid-19 morbidity and mortality cleaning from NYT
#
# this script loads the raw data from the New York Times Covid tracker.
# processes and cleans it and saves it as Rds file in the processed_data folder
# processing done by Chloe Burjack

#load needed packages. make sure they are installed.
library(tidyverse)
library(readr)
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths
data_spot7 <- here::here("data","raw_data","time_series_covid19_confirmed_US.csv") 
data_spot8 <- here::here("data","raw_data","time_series_covid19_deaths_US.csv") 

#load data 
case <- read.csv(data_spot7)
death <- read.csv(data_spot8)

# removing variables
case [ , "iso2"] <- NULL
case [ , "iso3"] <- NULL
case [ , "code3"] <- NULL
case [ , "Lat"] <- NULL
case [ , "Long_"] <- NULL
case [ , "Country_Region"] <- NULL
death [ , "iso2"] <- NULL
death [ , "iso3"] <- NULL
death [ , "code3"] <- NULL
death [ , "Lat"] <- NULL
death [ , "Long_"] <- NULL
death [ , "Country_Region"] <- NULL
death [ , "Population"] <- NULL

# setting time period to start from Jan 1 2020 - Dec 2021 31st
casesum <- case[,-c(716:754) ]
deathsum <- death[,-c(716:759) ]

# creating cumulative count
casesum["CumulativeCase"] <- rowSums(casesum[,6:715])
deathsum["CumulativeDeath"] <- rowSums(deathsum[,6:715])

# removing date columns
cleancasesum <- casesum [, -c(6:715)]
cleandeathsum <- deathsum[, -c(6:715)]

# joining the two sets
cumulativecovid <-left_join(cleancasesum, cleandeathsum[ , c("UID", "CumulativeDeath")], by='UID')

# renaming variable
cumulativecovid  <- rename(cumulativecovid , 
                           State = Province_State,
                           County = Admin2)


# Save   --------------------------------------------------------------------------------
save_data_location2 <- here::here("data","processed_data","covid.rds")
saveRDS(cumulativecovid, file = save_data_location2)



################################################################################
## California Hosp data
https://data.ca.gov/dataset/covid-19-hospital-data1/resource/0d9be83b-5027-41ff-97b2-6ca70238d778

data_spot10 <- here::here("data","raw_data","California Hosp. data.csv") 
#load data 
cacov <- read.csv(data_spot10)
summary(cacov)

# converting into date format
cacov1 <- cacov %>%
  mutate(date = as.Date(todays_date, "%m/%d/%y")) %>% select(-c(todays_date))
str(cacov1)
summary(cacov1)

# trimming within our date range
cacov2 <- cacov1[cacov1$date <= "2021-12-31", ]
summary(cacov2)
cacov2 <- cacov2 %>% select(-c(date)) # removing date column because we do not need it anymore


# computing cumulative hospitalization and icu_covid_confirmed_ patients
cacov3 <- cacov2 %>% group_by(county) %>%  summarise_if(is.numeric, sum, na.rm = TRUE) 
summary(cacov3) 

#### CHECK THIS OUT ^^^^^^^^^^^^


# taking only important variables
cacov4 <- cacov3 %>% select(c(county, hospitalized_covid_patients, icu_covid_confirmed_patients))


# Save   --------------------------------------------------------------------------------
save_data_location3 <- here::here("data","processed_data","CAcovid.rds")
saveRDS(cacov4, file = save_data_location3)


