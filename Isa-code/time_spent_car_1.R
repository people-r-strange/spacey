library(tidyverse)
library(knitr)
library(plotly)
#load sensor data

sensor_data <- read_csv("data/Lekagul Sensor Data.csv")

#renaming columnns
names(sensor_data)[1] <- "DateTime"
names(sensor_data)[2] <- "ID"
names(sensor_data)[3] <- "CarType"
names(sensor_data)[4] <- "GateName"

#how much time each car type spent at each location 
cars_time_spent <- sensor_data %>% group_by(ID) %>% mutate(time_spent = difftime(lead(DateTime),DateTime, unit = "mins"))

no_na <- cars_time_spent %>% drop_na() #nas represent the exit 

spent <- no_na %>% group_by(CarType,GateName) %>% mutate(average_time = mean(time_spent), sd = sd(time_spent)) %>% select(GateName, CarType, average_time, sd)

#where do cars go?
unique_cars_time <- distinct(spent)

#camping only 
camping_time_spent <- dplyr::filter(cars_time_spent, grepl('camping', GateName))

#ranger only 
ranger_time_spent <- dplyr::filter(cars_time_spent, grepl('ranger', GateName))

#general gate only 
general_gate_time_spent <- dplyr::filter(cars_time_spent, grepl('general-gate', GateName))

#visualization average time spent in each gate
ggplot(unique_cars_time, aes(x = GateName , y = average_time, col = CarType)) +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#variation of time spent in camping site. 
ggplot(camping_time_spent, aes(x = GateName, y = time_spent, col = CarType)) +
  geom_boxplot() +
  labs(title = "Time Spent in Camping Sites by Car Type", y = "Time (in minutes)", x = "Gate Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#variation of time spent in general gate stations
ggplot(general_gate_time_spent, aes(x = GateName, y = time_spent, col = CarType)) +
  geom_boxplot() +
  labs(title = "Time Spent in General Gate Sites by Car Type", y = "Time (in minutes)", x = "Gate Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#variation of time spent in ranger stations
ggplot(ranger_time_spent, aes(x = GateName, y = time_spent, col = CarType)) +
  geom_boxplot() +
  labs(title = "Time Spent in Ranger Sites by Car Type", y = "Time (in minutes)", x = "Gate Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
# who is visiting ranger stops 1 and 3?
suspicious_rangers_1 <- sensor_data %>% filter(CarType != "2P", GateName == "ranger-stop1") %>% mutate(time = format(DateTime, "%H:%M"))
suspicious_rangers_3 <- sensor_data %>% filter(CarType != "2P", GateName == "ranger-stop3") %>% mutate(date = format(DateTime, "%Y-%m"), time = format(DateTime, "%H:%M"))


#visualization of ranger stop 1 
range1 <- ggplot(suspicious_rangers_1, aes(x = time, y = ID, col = ID)) +
  geom_point() +
  labs(title = "July 10th, 2015: Ranger Stop 1 Non-Ranger Movements", x = "Time (Military)", y = "Car ID")

ggplotly(range1)

  
#visualization of ranger stop 3
range3 <- ggplot(suspicious_rangers_3, aes(x = date, y = time, col = ID)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Times Trucks enter Ranger Stop 3", y = "Time (Military)", x = "Date")

ggplotly(range3)

kable(suspicious_rangers_3)

#where were the trucks beforehand?
truck_id <- sensor_data %>% filter(ID %in% suspicious_rangers_3$ID) %>% group_by(ID) %>% mutate(num_trucks = n()) %>% ungroup() %>% group_by(GateName) %>% mutate(num_times_gate = n())

#how much time did each ID spend in the park? 
id_time_spent <- sensor_data %>% group_by(ID) %>% mutate(time_spent = difftime(tail(DateTime, n = 1L), DateTime[1], unit = "mins"))

#visualizing how much time spent 
id_time <- ggplot(id_time_spent, aes(y = time_spent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Time Spent", y = "Time (Min, Military)", x = "Car Type")

ggplotly(id_time)

#this box plot is horrible but we have 60 minutes cruisers, 206 minutes (3 hours) day people, 3107 minutes (2 days) weekenders, 



#Time series by month-year
sensor_years <- sensor_data %>%
  mutate(date = format(DateTime, "%Y-%m")) %>% 
    group_by(date, GateName) %>% mutate(num_cars = n())
        
        
#time series visual- not sure how this helps but alas
ggplot(sensor_years, aes(x = date, y = num_cars)) +
  geom_line() +
  labs(x = 'Date', y = 'Number of Cars', title = "Total Numbers of Visitors TD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(angle = 30))
        
        
        
#what is the most common time for cars to enter a gate
common_hour <- sensor_data %>% mutate(hour = as.numeric(format(DateTime,"%H"))) %>% group_by(GateName, CarType, hour) %>%  summarize(frequency = n()) %>% filter(frequency == max(frequency))


#visualization 
ggplot(common_hour, aes(x = GateName, y = hour, col = CarType)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
        
        
        

