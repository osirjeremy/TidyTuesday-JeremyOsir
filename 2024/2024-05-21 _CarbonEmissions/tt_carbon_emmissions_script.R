#Load libraries for EDA and data viz
library(skimr)
library(tidyverse) # loads ggplot2, tidyr, dplyr, and several other EDA packages
library(tibble)

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


# Faceted area chart - emmissions by different entities over time
ggplot(emissions_by_entity_type, 
       aes(x = year, y = total_emissions_entity, fill = parent_type)) +
  geom_area(alpha = 0.4) +
  geom_line(aes(color = parent_type), size = 0.5) + # Add geom_line for trends
  labs(title = "Total CO2 Emissions for Different Entities Over Time",
       x = "Year", y = "Total Emissions (MtC02e)") +
  facet_wrap(~ parent_type, scales = "fixed")

# Some questions - over time, which are the top 5 entities responsible for emissions for each entity category

emitters_top10 <- emissions %>% 
  group_by(parent_entity) %>% 
  summarise(total_emissions_alltime = sum(total_emissions_MtCO2e)) %>% 
  arrange(desc(total_emissions_alltime)) %>% 
  slice_head(n=10) %>% 
  rename(`Parent Entity` = parent_entity,
         `Total Emissions (MtCO2e)` = total_emissions_alltime)
#   mutate(`Total Emissions (MtCO2e)` = scales::comma(`Total Emissions (MtCO2e)`))
# 
# emitters_top5

# Create a barplot
top10_emitters <- ggplot(emitters_top10, aes(x = reorder(`Parent Entity`, -`Total Emissions (MtCO2e)`), 
                          y = `Total Emissions (MtCO2e)`)) +
  geom_bar(stat = "identity", fill = "#4B8BBE", color = "black") +
  geom_text(aes(label = scales::comma(`Total Emissions (MtCO2e)`)), 
            vjust = -0.3, size = 3.8, color = "black") +
  labs(title = "Top 10 Carbon Dioxide Emitters, 1854 - 2022",
       subtitle = "Units - million tonnes of carbon dioxide equivalent (MtCO2e)",
       caption = "Source: Carbon Majors Emissions Database",
       x = "Parent Entity",
       y = "Total Emissions (MtCO2e)") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 5)),
    axis.title.y = element_text(size = 12, face = "bold",margin = margin(r = 10)),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust =1.2, face = "bold"),
    panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid', colour = "gray"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# create a folder in working directory to save the plots
dir.create("plots")

# save plots in folder for Git Repo

ggsave("plots/top10_emitters.png", plot = top10_emitters, width = 10, height = 7, units = "in", dpi = 300, bg = "white")

# Print the path where the plot is saved
print("Plot saved to: plots/top10_emitters_barplot.png")
