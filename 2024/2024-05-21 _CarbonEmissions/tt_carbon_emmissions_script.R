#Load libraries for EDA and data viz
library(skimr)
library(tidyverse) # loads ggplot2, tidyr, dplyr, and several other EDA packages

#set working directory - this is where the project files will be saved
setwd("./Desktop/D_Sci/tidyTuesdays/2024/2024-05-21 _CarbonEmissions")


# Load data from github
emissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv')
write.csv(emissions, file = "carbon_emissions_sources.csv", row.names = FALSE)

# Skim dataframe
skimr::skim(emissions)

# Some questions to explore
# Overall, how have emmissions changed over time?
  # How have emissions from different commodities changed over time?
  # How have emissions from different entities changed over time?


#1. Visualizing total emissions of CO2 over time

emissions_by_year <- emissions %>% 
  group_by(., year) %>% 
  summarise(total_emissions_annual = sum(total_emissions_MtCO2e))

emissions_by_year # this df shows cumulative emissions each year for all entities

# Line Chart - total CO2 emissions over time
ggplot(emissions_by_year, aes(x = year, y = total_emissions_annual)) +
  geom_line(color = "red") +
  labs(title = "Total CO2 Emissions over time",
       x = "year", y = "Total Emissions (MtC02e)")
  
#2. Visualizing total emissions of CO2 over time for different commodities

emissions_annual_by_commodity <- emissions %>% 
  group_by(year, commodity) %>% 
  summarise(total_emissions_commodity = sum(total_emissions_MtCO2e))
  
# Line Chart - total CO2 emissions over time
ggplot(emissions_annual_by_commodity, 
       aes(x = year, y = total_emissions_commodity, color = commodity)) +
    geom_line() +
    labs(title = "Total CO2 Emissions for Different Commodities Over Time",
         x = "Year", y = "Total Emissions (MtC02e)")
  
# Area chart
ggplot(emissions_annual_by_commodity, 
       aes(x = year, y = total_emissions_commodity, fill = commodity)) +
  geom_area(alpha = 0.6) +
  labs(title = "Total CO2 Emissions for Different Commodities Over Time",
       x = "Year", y = "Total Emissions (MtC02e)")


#2. Visualizing total emissions of CO2 over time for different entities

emissions_by_entity_type <- emissions %>% 
  group_by(year, parent_type) %>% 
  summarise(total_emissions_entity = sum(total_emissions_MtCO2e))

# Line Chart - 
  # Nation States and State-Owned entities emit the most.
  # Emissions from investor-owned entities seems to be decreasing.
ggplot(emissions_by_entity_type, 
       aes(x = year, y = total_emissions_entity, color = parent_type)) +
  geom_line() +
  labs(title = "Total CO2 Emissions by Different Entities Over Time",
       x = "Year", y = "Total Emissions (MtC02e)")

# Area chart 
  # It's quite hard to tell which entities produce the most when the areas are stacked
  # Creating seperate charts for each entity type is clearer
ggplot(emissions_by_entity_type, 
       aes(x = year, y = total_emissions_entity, fill = parent_type)) +
  geom_area(alpha = 0.4) +
  labs(title = "Total CO2 Emissions for Different Entities Over Time",
       x = "Year", y = "Total Emissions (MtC02e)")


# Faceted area chart
ggplot(emissions_by_entity_type, 
       aes(x = year, y = total_emissions_entity, fill = parent_type)) +
  geom_area(alpha = 0.4) +
  geom_line(aes(color = parent_type), size = 0.5) + # Add geom_line for trends
  labs(title = "Total CO2 Emissions for Different Entities Over Time",
       x = "Year", y = "Total Emissions (MtC02e)") +
  facet_wrap(~ parent_type, scales = "fixed")

