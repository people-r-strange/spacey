library(tidyverse)
library(knitr)

#load in Data
Lekagul_Sensor_Data <- read_csv("~/Documents/Spring2021/Visual Analytics/Spacey/DC3-data/Traffic Data/Lekagul Sensor Data.csv")

#renaming columns 
names(Lekagul_Sensor_Data)[1] <- "DateTime"
names(Lekagul_Sensor_Data)[2] <- "ID"
names(Lekagul_Sensor_Data)[3] <- "CarType"
names(Lekagul_Sensor_Data)[4] <- "GateName"

#arrange data by ID 
arranged_data_by_ID <- arrange(Lekagul_Sensor_Data, ID, .by_group = TRUE)

#number of minutes each ID spent at a gate 
minutes_spent_gate <- arranged_data_by_ID %>% 
  group_by(ID) %>% 
  mutate(time_spent_minutes = difftime(lead(DateTime), DateTime, unit = "mins")) %>% 
  drop_na()

#Total number of minutes each ID spent in the park 
total_duration <- aggregate(time_spent_minutes~CarType+ID, data=minutes_spent_gate, FUN=sum) 

#Duration
duration_grouped <- total_duration %>%
  group_by(time_spent_minutes) %>% mutate(num_cars = n())
--------------------------------------------------------------------------------------------------------
#Categorize by Type 
day_trippers <- subset(total_duration, time_spent_minutes <= 24) 
day_trippers <-  day_trippers %>% 
  group_by(CarType) %>% 
  mutate(count = n()) %>%
  drop_na()

Month <- subset(total_duration, time_spent_minutes > 24 & time_spent_minutes < 43800)
Month <-  Month %>% 
  group_by(CarType) %>% 
  mutate(count = n()) %>%
  drop_na()

Six_Month <- subset(total_duration, time_spent_minutes > 43800 & time_spent_minutes < 262800)
Six_Month <-  Six_Month %>% 
  group_by(CarType) %>% 
  mutate(count = n()) %>%
  drop_na()

over_6_month <- subset(total_duration, time_spent_minutes > 262800)
over_6_month <-  over_6_month %>% 
  group_by(CarType) %>% 
  mutate(count = n()) %>%
  drop_na()

Month_one_and_two <- subset(total_duration, time_spent_minutes > 43800 & time_spent_minutes < 87600)
Month_one_and_two <- Month_one_and_two %>% 
  group_by(CarType) %>% 
  mutate(count = n()) %>%
  drop_na()

Month_two_and_three <- subset(total_duration, time_spent_minutes > 87600 & time_spent_minutes < 131400)
Month_two_and_three <- Month_two_and_three %>% 
  group_by(CarType) %>% 
  mutate(count = n()) %>%
  drop_na()

Month_three_and_four <- subset(total_duration, time_spent_minutes > 131400 & time_spent_minutes < 175200)
Month_three_and_four <- Month_three_and_four %>% 
  group_by(CarType) %>% 
  mutate(count = n()) %>%
  drop_na()

Month_three_and_four <- subset(total_duration, time_spent_minutes > 175200 & time_spent_minutes < 525600 )
Month_three_and_four <- Month_three_and_four %>% 
  group_by(CarType) %>% 
  mutate(count = n()) %>%
  drop_na()

#One_Day_and_One_Week <- subset(total_duration, time_spent_minutes > 24 & time_spent_minutes < 10080)
#Week_one_and_two <- subset(total_duration, time_spent_minutes > 10080 & time_spent_minutes < 20160)
#Week_two_and_three <- subset(total_duration, time_spent_minutes > 20160 & time_spent_minutes < 30240)
#Week_three_and_four <- subset(total_duration, time_spent_minutes > 30240 & time_spent_minutes < 40320)
#Four_Week_and_One_Month <- subset(total_duration, time_spent_minutes > 40320 & time_spent_minutes < 43800)

---------------------------------------------------------------------------------------------------------
#Visualize duration 
 
  ##DAYTRIPPERS
ggplot(day_trippers, aes(x = CarType, fill = CarType)) +
  geom_bar() +
  theme(axis.text.x = element_text()) +
  labs(title = "Day Trippers: Vehicles in the park for one day", 
       y = "Count", 
       x = "CarType",
       color = "Legend") +
  scale_fill_discrete(
    name = "Car Type",
    labels=c("2 axle car (or motorcycle)",
                               "2 axle truck",
                               "Park Ranger",
                               "3 axle truck",
                               "4 axle (and above) truck",
                               "2 axle bus",
                               "3 axle bus")) +
  theme_light()


##MONTH
ggplot(Month, aes(x = CarType, fill = CarType)) +
  geom_bar() +
  theme(axis.text.x = element_text()) +
  labs(title = "Month-Long: Vehicles in the park for one month", y = "Count", x = "CarType") +
  scale_fill_discrete(
    name = "Car Type",
    labels=c("2 axle car (or motorcycle)",
             "2 axle truck",
             "Park Ranger",
             "3 axle truck",
             "4 axle (and above) truck",
             "2 axle bus",
             "3 axle bus")) +
  theme_light()

##6 MONTH
ggplot(Six_Month, aes(x = CarType, fill = CarType)) +
  geom_bar() +
  theme(axis.text.x = element_text()) +
  labs(title = "6 Month: Vehicles in the park for more less than 6 Months", y = "Count", x = "CarType") +
  scale_fill_discrete(
    name = "Car Type",
    labels=c("2 axle car (or motorcycle)",
             "2 axle truck",
             "Park Ranger",
             "3 axle truck",
             "4 axle (and above) truck",
             "2 axle bus",
             "3 axle bus")) +
  theme_light()

#OVER 6 MONTH
ggplot(over_6_month, aes(x = ID, y = time_spent_minutes, fill = CarType)) +
  geom_point(size = 4, color = "salmon") +
  theme(axis.text.x = element_text()) +
  labs(title = "Over 6 Months: Suspicious Vehicle in Park for 11.5 Months", y = "Time in Minutes", x = "ID") +
  scale_fill_discrete(
    name = "Car Type",
    labels=c("2 axle car (or motorcycle)")) +
  theme_light()













#Visualize car types and duration 
duration <- ggplot(total_duration, aes(x =  , y = as.numeric(time_spent_minutes))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Duration in Minutes of Each ID in the park", y = "Time (Military)", x = "ID")

#Grouped by ID: Time spent at each station, number of cars in each group IDs, number of times cars pass each gate 
grouped_id <- minutes_spent_gate %>% group_by(ID) %>% mutate(num_cars = n()) %>% ungroup() %>% group_by(GateName) %>% mutate(num_times_gate = n())%>% drop_na()

time_spent_gate <- sensor_data %>% group_by(ID) %>% mutate(time_spent_minutes = difftime(lead(DateTime),DateTime, unit = "mins"))

drop_na <- time_spent_gate %>% drop_na()


id_time_spent <- sensor_data %>% group_by(ID) %>% mutate(time_spent = difftime(tail(DateTime, n = 1L), DateTime[1], unit = "mins"))