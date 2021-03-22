library(tidyverse)
library(readxl)
library(knitr)
#load sensor data
chemical_data <- read_excel("data/Sensor Data.xlsx")
#renaming columnns
names(chemical_data)[3] <- "DateTime"

#look at the dates when the trucks were there... 
#from suspsicious rangers 3 table the dates in question are:
dates <- c("2016-02", "2016-03", "2016-05", "2016-05",
"2016-05")


chemical_dates <- chemical_data %>% mutate(date = format(DateTime, "%Y-%m"))

chemical_means <- chemical_dates %>% group_by(Chemical, Monitor, date) %>% mutate(average_reading = mean(Reading))

#visualization of ranger stop 1 
range1 <- ggplot(suspicious_rangers_1, aes(x = time, y = ID, col = ID)) +
  geom_point() +
  labs(title = "July 10th, 2015: Ranger Stop 1 Non-Ranger Movements", x = "Time (Military)", y = "Car ID")

ggplotly(range1)
