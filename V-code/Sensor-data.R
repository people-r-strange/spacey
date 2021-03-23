library(readr)
library(dplyr)
library(writexl)
library(tidyverse)

#load in sensor data
Lekagul_Sensor_Data <- read_csv("~/Documents/Spring2021/Visual Analytics/Spacey/DC3-data/Traffic Data/Lekagul Sensor Data.csv")

#renaming columns 
names(Lekagul_Sensor_Data)[1] <- "DateTime"
names(Lekagul_Sensor_Data)[2] <- "ID"
names(Lekagul_Sensor_Data)[3] <- "CarType"
names(Lekagul_Sensor_Data)[4] <- "GateName"

#filter out park rangers 
park_rangers <- filter(Lekagul_Sensor_Data, CarType == '2P')

park_rangers_count <- n_distinct(park_rangers$ID)

#filter out trucks 
 
trucks <- filter(Lekagul_Sensor_Data, CarType == '3')

trucks_count <- n_distinct(trucks$ID)
  
#filter out any vehicles passing through gates that aren't park_rangers
gate <- c('gate1', 'gate2', 'gate3', 'gate4', 'gate5', 'gate6', 'gate7', 'gate8')

sus_gate_vehicles <- Lekagul_Sensor_Data %>%
  filter(CarType != '2P') %>%
  filter(GateName == gate)

#grouping IDs within their movement at the Gate
sus_gate_vehicles <- arrange(sus_gate_vehicles, ID, .by_group = TRUE)

#zeroing in on the sus IDs 
sus_ID <- unique(sus_gate_vehicles[c("ID")])

#visualizing the sus IDs
ggplot(sus_gate_vehicles, aes(x=ID, fill = GateName)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  labs(title = "Non-Park Ranger Vehicles at Park Ranger Gates", y = "Count", x = "CarType")

#create vector of our sus_IDs
sus_ID_group <- c('20154702044723-914',
                  '20150416040441-902',
                  '20152925022919-735',
                  '20154907044911-419',
                  '20151520021556-881',
                  '20151201031245-77',
                  '20150920030917-854',
                  '20151112031119-409',
                  '20155201025245-696',
                  '20162401032410-101',
                  '20162419042411-322')

#arrange data by ID 
arranged_data_by_ID <- arrange(Lekagul_Sensor_Data, ID, .by_group = TRUE)

#unique IDs
total_vehicle_count <- n_distinct(arranged_data_by_ID$ID)

visitor_count = (total_vehicle_count - park_rangers_count) 

#filter out visitors 
visitor_vehicles <- filter(Lekagul_Sensor_Data, CarType != '2P')

#arrange visitors by ID 
visitor_vehicles <- arrange(visitor_vehicles, ID, .by_group = TRUE)


#write excel files 
write_xlsx(park_rangers, "park_rangers.xlsx")

write_xlsx(sus_gate_vehicles, "sus_gate_vehicles.xlsx")

write_xlsx(visitor_vehicles, "visitor_vehicles.xlsx")

write_xlsx(arranged_data_by_ID,"arranged_data_by_ID.xlsx")

  