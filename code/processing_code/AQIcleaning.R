###############################
# Processing Script Processing Script #1 - AQI Data from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Annual
#
#this script loads the raw data from the EPA Air Data.
#processes and cleans it and saves it as Rds file in the processed_data folder

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
#From Dengue Forecasting Project
data_spot1 <- here::here("data","raw_data","annual_aqi_by_county_2011.csv") 
data_spot2 <- here::here("data","raw_data","annual_aqi_by_county_2012.csv") 
data_spot3 <- here::here("data","raw_data","annual_aqi_by_county_2013.csv")
data_spot4 <- here::here("data","raw_data","annual_aqi_by_county_2014.csv")
data_spot5 <- here::here("data","raw_data","annual_aqi_by_county_2015.csv")

#load data 
aqi11 <- read.csv(data_spot1)
aqi12 <- read.csv(data_spot2)
aqi13 <- read.csv(data_spot3)
aqi14 <- read.csv(data_spot4)
aqi15 <- read.csv(data_spot5)

# Extracting only median AQI data
keeps <- c('State', 'County', 'Median.AQI')
aqi11 = aqi11[keeps]
aqi12 = aqi12[keeps]
aqi13 = aqi13[keeps]
aqi14 = aqi14[keeps]
aqi15 = aqi15[keeps]

#renaming before merging
colnames(aqi11)[colnames(aqi11) == 'Median.AQI'] <- 'aqi2011' 
colnames(aqi12)[colnames(aqi12) == 'Median.AQI'] <- 'aqi2012'
colnames(aqi13)[colnames(aqi13) == 'Median.AQI'] <- 'aqi2013'
colnames(aqi14)[colnames(aqi14) == 'Median.AQI'] <- 'aqi2014'
colnames(aqi15)[colnames(aqi15) == 'Median.AQI'] <- 'aqi2015'

# Merging
t1<- left_join(aqi11,aqi12,by=c("State","County"))
t2<- left_join(t1,aqi13,by=c("State","County"))
t3<- left_join(t2,aqi14,by=c("State","County"))
final<- left_join(t3,aqi15,by=c("State","County"))

# Adding FIPPS code with usmap package
# Currently getting error message because there are some states not represented in package dataset. Will wait to merge by name with COVID data to get FIPPS code
final %>% 
  group_by(State) %>% 
  summarise(new = list(fips(state = first(State), county = County))) %>%
  unnest(c(new))

# Now to create average AQI column
final$av.aqi <- rowMeans(final[ , c(3:7)], na.rm=TRUE)
str(final)

