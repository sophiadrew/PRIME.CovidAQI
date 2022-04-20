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
# removing names
covid2 <- covid1[!(covid1$State=="Guam" | covid1$State=="Grand Princess" | covid1$State=="American Somoa" | 
                 covid1$State=="Diamond Princess"| covid1$State=="Northern Mariana Islands"| covid1$State=="Virgin Islands"),]

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
try1 <- merge(covid2,demo1,by=c("State","FIPS"), all = TRUE)
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
summary(fullper$State)

#fullper <- fullper[!(covid1$State=="Guam" | covid1$State=="Grand Princess" | covid1$State=="American Somoa" | 
                     #covid1$State=="Diamond Princess"| covid1$State=="Northern Mariana Islands"| covid1$State=="Virgin Islands"),]



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
caseratecountymap2 + theme_bw() +
  theme(axis.line = element_line(colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

caseratecountymap2
#save
ggsave(caseratecountymap2, filename = here("results", "caseratecountymap2.png"))

# correlation plot
library(corrplot)
library("Hmisc")

fullpermat <- fullper %>% select(av.aqi:rhisp) #rCumulativeCase,rCumulativeDeath, rTotalPop, male, female, under_18, age18to34, age35to64, age65, not_hispanic, white, black, AIANA, asian, pacific_island, other, mult_race, hisp, av.aqi)
fullpermat.cor <- Hmisc::rcorr(na.omit(as.matrix(fullpermat)))
corrplot(fullpermat.cor$r, type="upper", order="hclust", 
         p.mat = fullpermat.cor$P, sig.level = 0.05, insig = "blank")
fullpermat.cor
#save
ggsave(fullpermat.cor, filename = here("results", "fullpermat.cor.png"))


# creating basic table by STATE 
fullperstate<- full 
fullperstate <- fullperstate %>%
  group_by(State) %>% 
  summarise(  
  TotalPopulation = sum(TotalPop, na.rm = T),
  SCumulativeCase = sum(CumulativeCase, na.rm = T), 
  SCumulativeDeath = sum(CumulativeDeath, na.rm = T),
  DeathPerCase = (SCumulativeDeath/SCumulativeCase)*100,
  DeathRate = SCumulativeDeath/TotalPopulation, #dont like this, looks funky and possibly misleading.
  Male = (sum(male, na.rm = T)/TotalPopulation)*100,
  Under_18 = (sum(under_18, na.rm = T)/TotalPopulation)*100,
  Age18to34 = (sum(age18to34, na.rm = T)/TotalPopulation)*100, 
  Age35to64 = (sum(age35to64, na.rm = T)/TotalPopulation)*100,  
  Age65 = (sum(age65, na.rm = T)/TotalPopulation)*100,  
  Not_hispanic = (sum(not_hispanic, na.rm = T)/TotalPopulation)*100,  
  White  = (sum(white, na.rm = T)/TotalPopulation)*100, 
  Black = (sum(black, na.rm = T)/TotalPopulation)*100, 
  AIANA = (sum(AIANA, na.rm = T)/TotalPopulation)*100,   
  Asian = (sum(asian, na.rm = T)/TotalPopulation)*100,   
  Pacific_island = (sum(pacific_island, na.rm = T)/TotalPopulation)*100,   
  Other = (sum(other, na.rm = T)/TotalPopulation)*100,   
  Mult_race = (sum(mult_race, na.rm = T)/TotalPopulation)*100,     
  Hispanic = (sum(hisp, na.rm = T)/TotalPopulation)*100,
  AQI = mean(av.aqi,na.rm = T))    

# copy and paste into excel
fullperstate




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


################################################################################
################################################################################
################################################################################
#######################  CALIFORNIA ONLY  ######################################

ca <- subset(fullper, State == "California")
ca2 <- subset(dataall, State == "california") # for coordinate mapping
#removing non counties
ca <-ca[!(ca$County=="Unassigned" | ca$County=="Out of CA"),]
# we could also get rid of: alpine county, 
# keep in mind there are only 58 counties in CA
# this is our final frequency tbl

data_spot11 <- here::here("data","processed_data","CAcovid.rds") 
#load data 
CAcovid <- read_rds(data_spot11)
CAcovid <- rename(CAcovid, County = county) 

# merging with hospitalization
bigca <- merge(ca, CAcovid,by=c("County"), all = TRUE)
summary(bigca)

bigca1 <- bigca %>% mutate(
                    hosp = (hospitalized_covid_patients/TotalPop),
                    covidhosp = (hospitalized_covid_patients/CumulativeCase) *1000,
                    cfr = (CumulativeDeath/CumulativeCase) *1000,
                    icu = (icu_covid_confirmed_patients/TotalPop))

bigca1 # this is our final frequency tbl

# Save   --------------------------------------------------------------------------------
save_data_location5 <- here::here("data","processed_data","cafinal.rds")
saveRDS(bigca1, file = save_data_location5)


# Summary table ---------------------------------------------------------------------------
sum.gt <- bigca1 %>% 
  select("TotalPop", "CumulativeCase", "CumulativeDeath", "male", "female", "under_18", "age18to34", "age35to64", "age65", "not_hispanic", "white", "black", "AIANA",
          "asian", "pacific_island", "other", "mult_race", "hisp", "av.aqi") %>% 
  gtsummary::tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean}",
                                     "{median} ({p25} - {p75})", 
                                     "{min} -  {max}"),
    missing = "no") 
  bold_labels() %>%
  modify_header(label ~ "**Variable**") 
sum.gt

# now plotting GGPLOT mapping at cases/population by county
CAcaseratemap <- ca2 %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=rcase), color="gray70") +
  labs(title = "Cumulative Covid-19 Case rate by US County") +
  coord_map() + 
  scale_fill_continuous( low = "blue", high = "red", 
                         name = "Cumulative Case rate Per County")
CAcaseratemap + theme_bw() +
  theme(axis.line = element_line(colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

CAcaseratemap
#save
ggsave(CAcaseratemap, filename = here("results", "CAcaseratemap.png"))

# correlation plot
library(corrplot)
library("Hmisc")

countmat <- ca %>% select(CumulativeCase:av.aqi) #rCumulativeCase,rCumulativeDeath, rTotalPop, male, female, under_18, age18to34, age35to64, age65, not_hispanic, white, black, AIANA, asian, pacific_island, other, mult_race, hisp, av.aqi)
countmat.cor <- Hmisc::rcorr(na.omit(as.matrix(countmat)))
corrplot(countmat.cor$r, type="upper", order="hclust", 
         p.mat = countmat$P, sig.level = 0.05, insig = "blank")
countmat.cor
#save
ggsave(fullpermat.cor, filename = here("results", "fullpermat.cor.png"))


# mapping california using plot_usmap

bigca1$fips <-bigca1$FIPS
## aqi
AQIcamap <- plot_usmap(include = c("CA"), data = bigca1, values = "av.aqi", color = "white", regions = "counties") + 
  labs(title = "California AQI from 2015 to 2019") +
  scale_fill_continuous( low = "blue", high = "red", 
                         name = "Averge AQI score", label = scales::comma) +
  theme(panel.background = element_rect(color = "black"))
AQIcamap

## hospitalized
hospcamap <- plot_usmap(include = c("CA"), data = bigca1, values = "covidhosp", color = "white", regions = "counties") + 
  labs(title = "California Cumulative Hospitalization Rate per 1,000 cases") +
  scale_fill_continuous( low = "blue", high = "red", 
                         name = "Hospitalization Rate", label = scales::comma) +
  theme(panel.background = element_rect(color = "black"))
hospcamap
ggsave(hospcamap, filename = here("results", "hospcamap.png"))


## cfr
cfrcamap <- plot_usmap(include = c("CA"), data = bigca1, values = "cfr", color = "white", regions = "counties") + 
  labs(title = "California Case Fatality Rate per 1,000 cases") +
  scale_fill_continuous( low = "blue", high = "red", 
                         name = "CFR per 1,000 cases", label = scales::comma) +
  theme(panel.background = element_rect(color = "black"))
cfrcamap
ggsave(cfrcamap, filename = here("results", "cfrcamap.png"))


# avg aqi
hist(bigca1$av.aqi)

# hosp
hist(bigca1$covidhosp)
hist(bigca1$hospitalized_covid_patients)
hist(bigca1$hospitalized_covid_patients)
hist(log(bigca1$covidhosp))

# age
hist(log(bigca1$covidhosp))


#sex

chisq.test(bigca1$av.aqi)

chisq <- chisq.test(bigca1$hospitalized_covid_patients)
chisq



# corr plot
ratemat <- ca %>% select(av.aqi:rhisp) #rCumulativeCase,rCumulativeDeath, rTotalPop, male, female, under_18, age18to34, age35to64, age65, not_hispanic, white, black, AIANA, asian, pacific_island, other, mult_race, hisp, av.aqi)
ratemat.cor <- Hmisc::rcorr(na.omit(as.matrix(ratemat)))
corrplot(ratemat.cor$r, type="upper", order="hclust", 
         p.mat = ratemat.cor$P, sig.level = 0.05, insig = "blank")
ratemat.cor


av.aqi:rhisp

library(corrplot)


# next steps
what is relationship b/w hospitalization and covid19 
weather someone was hospitalized (dichotioms)
outline variables





# A descriptive analysis was conducted on all the data. Here, t test and one-way ANOVA test were used to compare proportion of cases (PCI), proportion of deaths (PCD), and CFR values between states categories. Pearson and Spearman correlation, as well as linear regression analysis were conducted to assess the association between AQI, comorbidities, and demographic data and the number of cases and deaths from COVID-19 per 100,000 people. We have used log-transformed PCI and PCD as a dependent variable and mean AQI, weighted distribution of population in each state (%), and number of previously mentioned diseases of interest in each population as independent variables. All the statistical analyses were 2-tailed and performed at a 5% significance level using SPSS 16 and Prism version 6 (GraphPad).








