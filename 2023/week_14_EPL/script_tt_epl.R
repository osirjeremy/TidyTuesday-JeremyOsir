# Get the Data

# Premier League match data from the 2021-2022 season = 380 games

# Read in with tidytuesdayR package 
# install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2023-04-04')
#tuesdata <- tidytuesdayR::tt_load(2023, week = 14)

soccer <- tuesdata$soccer

# load libraries
library(skimr)
library(tidyverse) # includes ggplot, dplyr,lubridate
library(paletteer) # colors

# skim the date
skim(soccer)
  # 380 obs, 22 columns
  # each row represents a match
  # no missing values

# Some questions to investigate
  # 1. Which referees were the most strict?
  # 2. Which teams were most likely to overturn a halftime deficit?
  # 3. Which teams conceded the most fouls?
  # 4. Which teams were fouled the most?


#### Referees ####

strict_refs <- soccer %>% 
  group_by(Referee) %>% 
  summarize(total_home_yellows = sum(HY),
            total_away_yellows = sum(AY),
            total_home_reds = sum(HR),
            total_away_reds = sum(AR),
            total_yellows = sum(HY, AY),
            total_reds = sum(HR, AR),
            total_cards = sum(total_yellows, total_reds),
            total_games_reffed = n()) %>% 
  arrange(., desc(total_cards))


# Control for number of games since some refs oversee more games
  # Include only refs who have officiated 10+ games
  # calculate new column that captures cards given on a per game basis

strict_refs_min10 <- strict_refs %>% 
  filter(total_games_reffed >= 10) %>% 
  mutate(home_yellows_per_game = total_home_yellows/total_games_reffed,
         away_yellows_per_game = total_away_yellows/total_games_reffed,
         yellows_per_game = (total_home_yellows +total_away_yellows)/total_games_reffed,
         home_reds_per_game = total_home_reds/total_games_reffed,
         away_reds_per_game = total_away_reds/total_games_reffed,
         reds_per_game = (total_home_reds +total_away_reds)/total_games_reffed,
         total_cards_per_game = total_cards/total_games_reffed)
view(strict_refs_min10)

# Create another dataframe that subsets per game statistics
strict_refs_plotting <- strict_refs_min10 %>% 
  select(c(Referee,home_yellows_per_game:total_cards_per_game))

#### DATAVIZ ####
 # Barplot

# average number of cards that each referee gave
strict_refs_min10 %>% 
  arrange(desc(total_cards_per_game)) %>% 
  ggplot(aes(x = reorder(Referee, +total_cards_per_game), y = total_cards_per_game)) + # reorder arranges bar in desc order
  geom_bar(stat = "identity", color = "yellow", fill = "red")  +
  coord_flip() 

# average number of yellows and reds given in a game
  # first reshape df from wide to long
strict_refs_long <- strict_refs_plotting %>%
  select(c(Referee, home_yellows_per_game, away_yellows_per_game)) %>% 
  pivot_longer(!Referee, names_to = "card_metric", values_to = "count") %>% 
  # arrange(Referee) %>%  # Ensure data is sorted by referee
  # group_by(Referee) %>%  # Group by referee
  # mutate(CumulativeTotal = cumsum(count)) %>%  # Calculate cumulative total
  # ungroup()  # Remove grouping

ref_yellow_plot <- strict_refs_long %>%
  ggplot(aes(x = reorder(Referee, +count), y = count, fill = card_metric)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "white") +
  geom_text(aes(label = ifelse(count == round(count), format(count, digits = 1), format(count, digits = 2))), 
            position = position_stack(vjust = 0.5), size = 3.5, color = "black", fontface = "bold") +
  labs(title = "Average Number of Yellow Cards Awarded by Each Premier League Referee",
       subtitle = "2021/2022 English Premier League Season",
       caption = "Minimum Number of Games Refereed: 10 games",
       x = "Referee",
       y = "Average Number of Yellow Cards per Game") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
        plot.caption.position = "panel",  # Corrected argument
        plot.caption = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(color = "grey20", face = "bold", size = 11),
        axis.title.y = element_text(face = "bold", size = 11),
        axis.text.y = element_blank(),  
        axis.text.x = element_text(face = "bold", size = 9, angle = 45, hjust = 1, vjust = 1.3, margin = margin(b = -5)),  # Rotate x-axis labels
        legend.position = "bottom") + 
  labs(fill = "") +
  scale_y_continuous(limits = c(0, 5)) +  
  scale_fill_manual(labels = c("Away Team", "Home Team"), values = c("#FFD700", "#FF8C00"))

ref_yellow_plot


# ggsave("fig_output/yellows_home_away.png", ref_yellow_plot, width = 10, height = 6, units = "in", dpi = 300)
ggsave("fig_output/yellows_home_away.png", ref_yellow_plot, width = 15, height = 6, units = "in", dpi = 300, bg = "white")

