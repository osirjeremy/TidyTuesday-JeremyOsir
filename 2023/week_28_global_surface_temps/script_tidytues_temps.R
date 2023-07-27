library(tidyverse)
library(paletteer)
library(skimr)
library(colorspace)

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
  pivot_longer(., cols = !Year, names_to = "month", values_to = "temp_deviation") %>% 
  mutate(., "hemisphere" = "global")

# convert northern hemisphere temp df to long format
nh_temps_long <- nh_temps %>% 
  pivot_longer(., cols = !Year, names_to = "month", values_to = "temp_deviation") %>% 
  mutate(., "hemisphere" = "north")

# convert southern hemisphere temp df to long format
sh_temps_long <- sh_temps %>% 
  pivot_longer(., cols = !Year, names_to = "month", values_to = "temp_deviation") %>% 
  mutate(., "hemisphere" = "south")


# combine dataframes vertically, using rbind




all_temps <- bind_rows(global_temps_long, nh_temps_long, sh_temps_long)



# Plot global temp deviation over time, by month 
all_temps_monthly <- all_temps %>% 
  filter(., month %in% c("Jan", "Feb", "Mar" ,"Apr", "May", "Jun" ,"Jul", "Aug" ,"Sep" ,"Oct", "Nov", "Dec")) %>% 
  ggplot(., aes(x = Year, y = temp_deviation, color = hemisphere)) +
  geom_jitter(alpha = 0.3, width = 0.2, height = 0.2) +
  geom_smooth() +
  scale_color_discrete_diverging(palette = "Blue-Red")
all_temps_monthly


# It is a little hard to see the points, let's switch to temp deviations by year (instead of month)

hem_colors <- c("global" = "#004586FF", "north" = "#FF420EFF", "south" = "#579D1CFF")

all_temps_annual <- all_temps %>% 
  filter(., month== "J-D") %>% 
  ggplot(aes(x = Year, y = temp_deviation, color = hemisphere)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(1880, 2023,10), 
                     expand = c(0.01,0)) + 
  scale_y_continuous(limits = c(-0.7, 1.5), 
                     expand = c(0,0)) +
  scale_color_manual(values = hem_colors)+
  labs(title = "Comparing Global Surface Temperature Changes in the Northern and Southern Hemispheres",
       subtitle = "Shown as deviation in degrees celsius from the 1951-1980 average",
       x = "Year",
       y = "Deviation from 1951-1980 average (°C) ", 
       caption = "Source: NASA Goddard Institute for Space Studies || Graphic: Jeremy Osir ") +
  theme_light()+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(size = 11, face = "italic"),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = c(0.1, 0.8))
  
all_temps_annual

ggsave("fig_output/global_temps_trend.png", all_temps_annual, width = 8, height = 6)
