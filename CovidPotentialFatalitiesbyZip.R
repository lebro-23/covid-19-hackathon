library(readr)
library(ggplot2)
library(sf)
library(stringr)
library(dplyr)
library(RColorBrewer)

#Read in datasets and and shapefile
covid_df <- read_csv("CoV_Lancet_DigitalHealth_Sun_Viboud - cov_reported_jan31.csv")
census_df <- read_csv("aff_download/ACS_17_5YR_S0101_with_ann.csv")

#Too big for github - Download from https://www2.census.gov/geo/tiger/TIGER2017/ZCTA5/ and put in directory
shp_file <- st_read("tl_2017_us_zcta510/tl_2017_us_zcta510.shp")

#Check datasets
#glimpse(covid_df)
#glimpse(census_df)

#Female Age Distribution
ggplot(covid_df %>% filter(gender=="female" & death == 1),aes(age)) + geom_histogram()

#Male Age Distribution
ggplot(covid_df %>% filter(gender=="male" & death == 1),aes(age)) + geom_histogram()

#Clean up column names
census_df_2 <- census_df
colnames(census_df_2) <- census_df_2[1,]
census_df_2 <- census_df_2[-1,]

#Check
#glimpse(census_df_2)

#Summarised dataset of total number of males by age group deciles
male_overall <- covid_df %>% 
  filter(gender=="male") %>% 
  mutate(age_rounded = round(age/10)*10) %>% 
  group_by(age_rounded) %>% 
  summarise(group_total = n())

#Summarised dataset of total number of males who died by age group deciles
male_deaths <- covid_df %>% 
  filter(gender=="male" & death==1) %>% 
  mutate(age_rounded = round(age/10)*10) %>% 
  group_by(age_rounded) %>% 
  summarise(group_deaths = n())

#Combine to get percent of deaths by age group
male_perc_df <- male_overall %>% 
  left_join(male_deaths) %>% 
  mutate(death_perc = group_deaths/group_total)

#Summarised dataset of total number of females by age group deciles
female_overall <- covid_df %>% 
  filter(gender=="female") %>% 
  mutate(age_rounded = round(age/10)*10) %>% 
  group_by(age_rounded) %>% 
  summarise(group_total = n())

#Summarised dataset of total number of females who died by age group deciles
female_deaths <- covid_df %>% 
  filter(gender=="female" & death==1) %>% 
  mutate(age_rounded = round(age/10)*10) %>% 
  group_by(age_rounded) %>% 
  summarise(group_deaths = n())

#Combine to get percent of deaths by age group
female_perc_df <- female_overall %>% 
  left_join(female_deaths) %>% 
  mutate(death_perc = group_deaths/group_total)

#Group census data into same age group deciles for each zipcode 
zipcode_age_60 <- as.numeric(census_df_2$`Male; Estimate; AGE - 55 to 59 years`) + as.numeric(census_df_2$`Male; Estimate; AGE - 60 to 64 years`)
zipcode_age_70 <- as.numeric(census_df_2$`Male; Estimate; AGE - 65 to 69 years`) + as.numeric(census_df_2$`Male; Estimate; AGE - 70 to 74 years`)
zipcode_age_80 <- as.numeric(census_df_2$`Male; Estimate; AGE - 75 to 79 years`) + as.numeric(census_df_2$`Male; Estimate; AGE - 80 to 84 years`)
zipcode_age_85_over <- as.numeric(census_df_2$`Male; Estimate; AGE - 85 years and over`)

zipcode_df <- data.frame(zipcode=census_df_2$Geography,
                         zipcode_age_60,
                         zipcode_age_70,
                         zipcode_age_80,
                         zipcode_age_85_over,
                         total_pop = as.numeric(census_df_2$`Total; Estimate; Total population`))

####################################################################################
#Parameters
MALE_INFECTION_RATE=0.3
FEMALE_INFECTION_RATE=0.3
####################################################################################

#Using percentage of death rates, extrapolate possible number of deaths in each zip code
zipcode_df$possible_deaths_60 <- MALE_INFECTION_RATE*zipcode_df$zipcode_age_60*male_perc_df[male_perc_df$age_rounded==60,"death_perc"] %>% na.omit %>% unlist
zipcode_df$possible_deaths_70 <- MALE_INFECTION_RATE*zipcode_df$zipcode_age_70*male_perc_df[male_perc_df$age_rounded==70,"death_perc"] %>% na.omit %>% unlist
zipcode_df$possible_deaths_80 <- MALE_INFECTION_RATE*zipcode_df$zipcode_age_80*male_perc_df[male_perc_df$age_rounded==80,"death_perc"] %>% na.omit %>% unlist
zipcode_df$possible_deaths_85_over <- MALE_INFECTION_RATE*zipcode_df$zipcode_age_85_over*((male_perc_df[male_perc_df$age_rounded==80,"death_perc"] %>% na.omit %>% unlist) + (male_perc_df[male_perc_df$age_rounded==90,"death_perc"] %>% na.omit %>% unlist))/2
zipcode_df$total_possible_deaths <- zipcode_df$possible_deaths_60 + zipcode_df$possible_deaths_70 + zipcode_df$possible_deaths_80 + zipcode_df$possible_deaths_85_over
zipcode_df$total_possible_deaths_perc <- zipcode_df$total_possible_deaths/zipcode_df$total_pop

#Sort by highest number of possible deaths
zipcode_df_sorted_total <- zipcode_df %>% arrange(desc(total_possible_deaths))

#Get the top 1000 zipcodes and corresponding shapefiles
top_1000_zips <- zipcode_df_sorted_total[0:1000,"zipcode"] %>% 
  as.character %>% 
  str_replace_all("ZCTA5 ","")
top_1000_shapes <- shp_file[as.character(shp_file$ZCTA5CE10) %in% top_1000_zips,]

#Create color scale to visualise 
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 1000)

#Plot (will take a while)
plot(top_1000_shapes$geometry,col = my_palette,border = my_palette)


