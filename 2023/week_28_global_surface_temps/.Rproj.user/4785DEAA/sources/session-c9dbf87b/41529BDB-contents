library(tidyverse)
library(paletteer)
library(skimr)

# load data
tuesdata <- tidytuesdayR::tt_load(2023, week = 28)

#Summary: This datasets are tables of global and hemispheric monthly means and zonal annual means. 
#         They combine land-surface, air and sea-surface water temperature anomalies (Land-Ocean Temperature Index, L-OTI). 
#         The values in the tables are deviations from the corresponding 1951-1980 means.


global_temps <- tuesdata$global_temps
nh_temps <- tuesdata$nh_temps
sh_temps <- tuesdata$sh_temps
zonann_temps <- tuesdata$zonann_temps

skim(global_temps)

# convert global temp df to long format
global_temps_long <- global_temps %>% 
  pivot_longer(., cols = !Year, names_to = "month", values_to = "temp_deviation")

# Plot global temp deviation over time, by month 
global_temps_monthly <- global_temps_long %>% 
  filter(., month %in% c("Jan", "Feb", "Mar" ,"Apr", "May", "Jun" ,"Jul", "Aug" ,"Sep" ,"Oct", "Nov", "Dec")) %>% 
  ggplot(., aes(x = Year, y = temp_deviation)) +
  geom_point() +
  geom_smooth()

global_temps_monthly  
  


