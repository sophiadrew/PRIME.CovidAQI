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

# setting time period to start from Jan 2020 - Dec 2021
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


