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


view(global_temps)

# Objective: create a bar chart showing the deviation in global annual surface temps

global_temps_df <- global_temps %>% 
  select(., c(year = Year,temp_diff = `J-D`))

view(global_temps_df)
