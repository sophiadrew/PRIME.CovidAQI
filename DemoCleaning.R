###############################
# Processing Script Processing Script #2 - Demographic data from https://www.socialexplorer.com/
#
# this script loads the raw data from the American Community Survey.
# processes and cleans it and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readr) #for loading Excel files & txt files
library(here) #to set paths
library(usmap)
library(dplyr) #for data processing
library(tidyr)
library(lubridate) #to arrange date form of data

#path to data
#note the use of the here() package and not absolute paths
data_spot6 <- here::here("data","raw_data","demodta.csv") 

#load data 
demo <- read.csv(data_spot6)

# splitting state from county for future merging
demo <- demo %>% 
  separate(Geo_QName,c("County","State"),sep=",")

# removing the word "county" from the data set
demo$County = gsub("County", "", demo$County)

# renaming multiple variables
demo1 <- rename(demo, TotalPop = SE_A00001_001,
       male = SE_A02001_002,
       female = SE_A02001_003,
       under_18 = SE_B01001_002,
       age18to34 = SE_B01001_003,
       age35to64 = SE_B01001_004,
       age65 = SE_B01001_005,
       not_hispanic = SE_B04001_002,
       white = SE_B04001_003,
       black = SE_B04001_004,
       AIANA  = SE_B04001_005,
       asian = SE_B04001_006,
       pacific_island = SE_B04001_007,
       other = SE_B04001_008,
       mult_race = SE_B04001_009,
       hisp = SE_B04001_010)

# removing unnecessary variables
demo1 <- demo1 %>% select(-c("Geo_FIPS",
                        "Geo_NAME", 
                        "Geo_STUSAB" ))

str(demo)


