library(tidyverse)
library(readxl)
library(knitr)
library(ggridges)
library(wesanderson)
#load sensor data
chemical_data <- read_excel("data/Sensor Data.xlsx")
#renaming columnns
names(chemical_data)[3] <- "DateTime"

#look at the dates when the trucks were there... 
#from suspsicious rangers 3 table the dates in question are:
dates <- c("2016-02", "2016-03", "2016-05", "2016-05",
           "2016-05")


chemical_dates <- chemical_data %>% 
  mutate(date = format(DateTime, "%Y-%m"))

chemical_means <- chemical_dates %>% 
  group_by(Chemical, Monitor, date) %>% 
  mutate(average_reading = mean(Reading))

daily_chemical_means <- chemical_means %>%
  select(Chemical, Monitor, date, average_reading)

daily_chemical_means <- daily_chemical_means %>% as.factor(Monitor)

#visualize the chemical data
ggplot( daily_chemical_means, aes(x=date, y= average_reading, fill=Chemical)) +
  geom_bar(stat="identity", position="dodge") +  
  labs(title = "Average Chemical Reading", y = "Average Reading", x = "Date") +
  scale_fill_discrete(
    name = "Chemical") +
  theme_light()

#focus on 2016-12
ggplot(daily_chemical_means, aes(x=as.factor(Monitor), y= average_reading, fill = Chemical)) + 
  facet_wrap( ~Chemical, ncol=2) + 
  geom_bar(stat="identity", position="dodge") +  
  labs(title = "Average Chemical Reading for December 2016", y = "Average Reading", x = "Monitor") +
  scale_fill_brewer(palette="Dark2")

#read in wind data
Meteorological_Data <- read_excel("~/Documents/Spring2021/Visual Analytics/Spacey/DC3-data/Sensor Data/Meteorological Data.xlsx")

#select first three columns 
Meteorological_Data <- Meteorological_Data %>% 
  select(1:3)

#separate date and time 
Meteorological_Data_sep <- separate(Meteorological_Data, Date, c("date", "time"), sep = " ")

Meteorological_Data_sep <- as.yearmon(date)
#
wind <- filter(Meteorological_Data_sep, date %in% c('2016-02', '2016-03', '2016-05', '2016-05',
           '2016-05')) 