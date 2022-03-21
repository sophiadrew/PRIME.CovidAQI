###############################
# EDA #1 
#
# this script loads the processed data and joins into one data set fro further exploration
# processes and cleans it and saves it as Rds file in the processed_data folder
# processing done by Sophia Drewry

#load needed packages. make sure they are installed.
library(readr) #for loading Excel files & txt files
library(here) #to set paths
library(usmap)
library(dplyr) #for data processing
library(tidyr)
library(lubridate) #to arrange date form of data
library(ggmap)
library(maps)

library(gtsummary) #for tables
library(gt)
library(janitor) #not used yet
library(kableExtra)
library(data.table)
library(flextable)
library(naniar)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)


# load datasets
data_spot7 <- here::here("data","processed_data","aqi.rds")
data_spot8 <- here::here("data","processed_data","covid.rds") 
data_spot9 <- here::here("data","processed_data","demo.rds") 
#load data 
aqi <- read_rds(data_spot7)
covid <- read_rds(data_spot8)
demo <- read_rds(data_spot9)

#turning all character into factor
covid1 <- covid %>% mutate_if(is.character,as.factor)
demo1 <- demo %>% mutate_if(is.character,as.factor)

# checking before merging
str(covid1$State) 
str(covid1$County) 
summary(covid1$State)
summary(covid1$County)

str(demo1$State) 
str(demo1$County) 
summary(demo1$State)
summary(demo1$County)

### find a better way to clean this
# fixing mis-matching before merging
# aqi$County <- aqi %>% filter(State == "Virginia") %>% gsub("Charles", "Charles City", aqi$County)
#remove variables before merging
demo1 <- demo1 %>% select(-c("County", 
                             "Geo_GEOID"))

################################# this needs work #############################
# merging datasets ------------------------------------------------------------
## Adding demo to covid set because covid reported the largest amount of states and counties
try1 <- merge(covid1,demo1,by=c("State","FIPS"), all = TRUE)
# now adding AQI to data set
full <- merge(try1, aqi,by=c("State","County"), all = TRUE)

fullper<- full
fullper <- fullper %>%
  mutate(
    rcase = CumulativeCase/TotalPop,
    rdeath = CumulativeDeath/TotalPop,
    rdeathpercase = CumulativeDeath/CumulativeCase,
    rmale = male/TotalPop,
    rfemale = female/TotalPop,
    runder_18 = under_18/TotalPop,
    rage18to34 = age18to34/TotalPop,
    rage35to64 = age35to64/TotalPop,
    rage65 = age65/TotalPop,
    rnot_hispanic  = not_hispanic/TotalPop,
    rwhite = white/TotalPop,
    rblack = black/TotalPop,
    rAIANA  = AIANA/TotalPop,    
    rasian = asian/TotalPop,
    rpacific_island = pacific_island/TotalPop,
    rother = other/TotalPop,
    rmult_race = mult_race/TotalPop,
    rhisp  = hisp/TotalPop)

# mapping at deaths per cases by county
fullper <- rename(fullper, fips = FIPS)
deathspercasecountymap <- plot_usmap(data = fullper, values = "rdeathpercase", color = "white", regions = "counties") + 
  labs(title = "Covid-19 Death per Case rate by US County") +
  scale_fill_continuous( low = "blue", high = "red", 
                         name = "Death Per County", label = scales::comma) +
  theme(panel.background = element_rect(color = "black"))
deathspercasecountymap
#save
ggsave(deathspercasecountymap, filename = here("results", "deathspercasecountymap.png"))

# mapping at cases/population by county <- odd metric but just for fun
caseratecountymap <- plot_usmap(data = fullper, values = "rcase", color = "white", regions = "counties") + 
  labs(title = "Covid-19 Case rate by US County") +
  scale_fill_continuous( low = "blue", high = "red", 
                         name = "Death Per County", label = scales::comma) +
  theme(panel.background = element_rect(color = "black"))
caseratecountymap
#save
ggsave(caseratecountymap, filename = here("results", "caseratecountymap.png"))

# adding lat/long for GGPLOT mapping
dfips <- maps::county.fips %>%
  as_tibble %>% 
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") 
dall<- map_data("county") %>% 
  left_join(dfips) 
dall<- rename(dall, County = subregion, State = region)
fullper$County <- tolower(fullper$County)
fullper$State <- tolower(fullper$State)
# joining and adding latlong to larger set, 
dataall<- fullper %>% 
  left_join(dall, by = c("fips", "State", "County"))


# now plotting GGPLOT mapping at cases/population by county
caseratecountymap2 <- dataall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=rcase), color="gray70") +
  labs(title = "Cumulative Covid-19 Case rate by US County") +
  coord_map() + 
  scale_fill_continuous( low = "blue", high = "red", 
                                       name = "Cumulative Case rate Per County")
caseratecountymap2 +
theme_bw() +
  theme(axis.line = element_line(colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

caseratecountymap2
#save
ggsave(caseratecountymap2, filename = here("results", "caseratecountymap2.png"))

untouched <- State, County, fips 


# creating basic table by STATE ----does not run
fullperstate<- full
fullperstate <- fullperstate %>%
  group_by(State) %>% 
  summarise(across(c(CumulativeCase,CumulativeDeath, TotalPop, male, female, under_18, age18to34, age35to64, age65, not_hispanic, white, black, AIANA, asian, pacific_island, other, mult_race, hisp), sum))

  summarise(across(vars(CumulativeCase:hisp), sum))

  summarise_at(which(sapply(is.numeric) & names(df) != 'Registered'), sum)
  summarise(across(everything(), list(sum)))
    
  t = fullperstate %>% group_by(State) %>%
    summarise_at(vars(CumulativeCase:hisp), fun=sum))
                   
    CumulativeCase = sum(CumulativeCase), 
            CumulativeDeath = sum(CumulativeDeath),
            deathpercase = sCumulativeDeath/CumulativeCase
    rcase = CumulativeCase/TotalPop,
    rdeath = CumulativeDeath/TotalPop,
    rdeathpercase = CumulativeDeath/CumulativeCase,
    rmale = male/TotalPop,
    rfemale = female/TotalPop,
    runder_18 = under_18/TotalPop,
    rage18to34 = age18to34/TotalPop,
    rage35to64 = age35to64/TotalPop,
    rage65 = age65/TotalPop,
    rnot_hispanic  = not_hispanic/TotalPop,
    rwhite = white/TotalPop,
    rblack = black/TotalPop,
    rAIANA  = AIANA/TotalPop,    
    rasian = asian/TotalPop,
    rpacific_island = pacific_island/TotalPop,
    rother = other/TotalPop,
    rmult_race = mult_race/TotalPop,
    rhisp  = hisp/TotalPop)
  
gtsummary::set_gtsummary_theme(my_theme)
demofull <- fullperstate %>%
  tbl_summary(by = State) %>% 
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  add_overall()
demofull
str(full)





# quick and dirty dataset  ------------------------------------------------------------
# this table has data from only counties with AQI data
sloppytry <- merge(covid1,demo1,by=c("State","FIPS"), all = TRUE)
# now adding AQI to data set
sloppyfull <- merge(aqi, sloppytry,by=c("State","County"), all = FALSE)
sloppyfull <- rename(sloppyfull, fips = FIPS)

# mapping at AQI data over a map
AQIcountymap <- plot_usmap(data = sloppyfull, values = "av.aqi", color = "white", regions = "counties") + 
  labs(title = "US Counties and average AQI from 2011 to 2015") +
       scale_fill_continuous( low = "blue", high = "red", 
                              name = "Averge AQI score", label = scales::comma) +
  theme(panel.background = element_rect(color = "black"))
AQIcountymap
ggsave(AQIcountymap, filename = here("results", "AQIcountymap.png"))


# creating demographic table
# code used from cody vape survey https://github.com/daileyco/Vape-Survey/blob/master/Analysis/COVID_MMWR.R
#SEX TABLE
count <- table(data$Q5, useNA = "always")
percent <- round(prop.table(table(data$Q5, useNA = "always")), 3)

cbind(count, percent)

table(data$Q5,data$covidall,useNA = "always")
round(prop.table(table(data$Q5,data$covidall, useNA = "always"), margin=1), 3) * 100
chisq.test(table(data$Q5,data$covidall))

tab <- epitab(table(data$Q5,data$covidall), method= "riskratio", rev= "rows")
round(tab$tab, 2)



# Save   --------------------------------------------------------------------------------
save_data_location2 <- here::here("data","processed_data","covid.rds")
saveRDS(cumulativecovid, file = save_data_location2)
