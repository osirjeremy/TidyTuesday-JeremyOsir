library(tidyverse)
library(paletteer)
library(skimr)
library(scales)
library(glue)

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
  select(., c(year = Year, temp_diff = `J-D`)) %>% 
  drop_na()


annotate_df <- global_temps_df %>% 
  slice(1, n()) %>% 
  mutate(temp_diff = 0,
         x = year +c(-5,5))

max_temp_diff <- max(global_temps_df$temp_diff)
min_year <- min(global_temps_df$year)

global_temps_barplot <- global_temps_df %>% 
  ggplot(aes(x = year, y = temp_diff, fill = temp_diff)) +
  geom_col() +
    # see Stackoverflow for arrow tips: https://stackoverflow.com/questions/38008863/how-to-draw-a-nice-arrow-in-ggplot2
  geom_segment(aes(x = 1885, y = -0.5, xend = 2015, yend=-0.5), arrow = arrow(length = unit(0.5, "cm")), color = "red") +
  
  #geom_text(data = annotate_df, aes(x = x, label = year), color = "white", size = 2) +
  # annotate("text", x = 1880, y = 1, hjust = 0, size = 0.6,
  #          label = glue("Global Temperatures have increased by {max_temp_diff}\u00B0C since {min_year} "),
  #          color = "white") +
  # annotate("text", x = 1880, y = 0.93, hjust = 0, size = 0.5, fontface = "italic",
  #          label = "Relative to the 1951-1980 average temperature ",
  #          color = "white") +
  
  labs(title = glue("Global Temperatures have increased by {max_temp_diff}\u00B0C since {min_year} "),
       subtitle = "Shown as deviation in degrees celsius from the 1951-1980 average", color = "white", 
       x = "YEAR", y = "TEMPERATURE CHANGE in \u00B0C "
        ) +
  scale_x_continuous(breaks = c(1880, 2022)) +
  # scale_fill_gradient2(low = "#172869FF", mid = "white", high = "darkred", midpoint = 0) +
  # scale_fill_gradientn(colors = c("#172869FF", "white", "darkred"),
  #                      values = rescale(c(min(global_temps_df$temp_diff), 0, max(global_temps_df$temp_diff))),
  #                      limits = c(min(global_temps_df$temp_diff), max(global_temps_df$temp_diff))) +
  scale_fill_stepsn(colors = c("#172869FF", "white", "darkred"),
                    values = rescale(c(min(global_temps_df$temp_diff), 0, max(global_temps_df$temp_diff))),
                    limits = c(min(global_temps_df$temp_diff), max(global_temps_df$temp_diff)),
                    n.breaks = 6) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "white", hjust = 0.05, size = 20),
        plot.subtitle = element_text(color = "white", hjust = 0.05, size = 16),
        axis.title.x = element_text(color = "white", vjust = 12, size = 14),
        axis.text.x = element_text(color = "white", vjust = 12, size = 14),
        axis.title.y = element_text(color = "white", angle = 90),
        legend.text = element_text(color = "white"))

global_temps_barplot

ggsave("./fig_output/temperature_bar_plot.png", height = 3, width = 5)


  labs(title = "Global Surface Temperatures are Rising Faster than Ever ",
       subtitle = "Shown as deviation in degrees celsius from the 1951-1980 average"
       caption = "Source: NASA Goddard Institute for Space Studies || Graphic by Jeremy Osir", 
       x = "Year",
       y = "Deviation from 1951-1980 average (°C)") +
  theme_void()

global_temps_barplot
