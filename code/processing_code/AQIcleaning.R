###############################
# Processing Script Processing Script #1 - AQI Data from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Annual
#
#this script loads the raw data from the EPA Air Data.
#processes and cleans it and saves it as Rds file in the processed_data folder
# processing done by Sophia Drewry

#load needed packages. make sure they are installed.
library(readr) #for loading Excel files & txt files
library(here) #to set paths
library(usmap)
library(dplyr) #for data processing
library(tidyr)
library(lubridate) #to arrange date form of data
library(weathermetrics) # Conversion
library(reshape2) # Reshaping data structure

#path to data
#note the use of the here() package and not absolute paths
data_spot1 <- here::here("data","raw_data","annual_aqi_by_county_2019.csv") 
data_spot2 <- here::here("data","raw_data","annual_aqi_by_county_2018.csv") 
data_spot3 <- here::here("data","raw_data","annual_aqi_by_county_2017.csv")
data_spot4 <- here::here("data","raw_data","annual_aqi_by_county_2016.csv")
data_spot5 <- here::here("data","raw_data","annual_aqi_by_county_2015.csv")

#load data 
aqi19 <- read.csv(data_spot1)
aqi18 <- read.csv(data_spot2)
aqi17 <- read.csv(data_spot3)
aqi16 <- read.csv(data_spot4)
aqi15 <- read.csv(data_spot5)

# Extracting only median AQI data
keeps <- c('State', 'County', 'Median.AQI')
aqi19 = aqi19[keeps]
aqi18 = aqi18[keeps]
aqi17 = aqi17[keeps]
aqi16 = aqi16[keeps]
aqi15 = aqi15[keeps]

#renaming before merging
colnames(aqi19)[colnames(aqi19) == 'Median.AQI'] <- 'aqi2019' 
colnames(aqi18)[colnames(aqi18) == 'Median.AQI'] <- 'aqi2018'
colnames(aqi17)[colnames(aqi17) == 'Median.AQI'] <- 'aqi2017'
colnames(aqi16)[colnames(aqi16) == 'Median.AQI'] <- 'aqi2016'
colnames(aqi15)[colnames(aqi15) == 'Median.AQI'] <- 'aqi2015'

# Merging
t1<- left_join(aqi19,aqi18,by=c("State","County"))
t2<- left_join(t1,aqi17,by=c("State","County"))
t3<- left_join(t2,aqi16,by=c("State","County"))
final<- left_join(t3,aqi15,by=c("State","County"))


# Now to create average AQI column
final$av.aqi <- rowMeans(final[ , c(3:7)], na.rm=TRUE)
str(final)

# remove variables
final <- final[,-c(3:7) ]


# Save   --------------------------------------------------------------------------------
save_data_location1 <- here::here("data","processed_data","aqi.csv")
write_csv(final, file = save_data_location1)

save_data_location1 <- here::here("data","processed_data","aqi.csv")
saveRDS(final, file = save_data_location1)