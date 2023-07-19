# Get the Data

# Premier League match data from the 2021-2022 season = 380 games

# Read in with tidytuesdayR package 
install.packages("tidytuesdayR")
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

view(strict_refs)  

# Control for number of games since some refs oversee more games, set minimum number of 
# games

view(strict_refs)

# Include only refs who have officiated 10+ games
# calculate new column that captures cards given on a per game basis

strict_refs_min10 <- strict_refs %>% 
  filter(total_games_reffed >= 10) %>% 
  mutate(home_yellows_per_game = total_home_yellows/total_games_reffed,
         home_reds_per_game = total_home_reds/total_games_reffed,
         away_yellows_per_game = total_away_yellows/total_games_reffed,
         away_reds_per_game = total_away_reds/total_games_reffed,
         total_cards_per_game = total_cards/total_games_reffed)
view(strict_refs_min10)


#### DATAVIZ ####
 # Barplot

# average number of cards that referee gives in a game
strict_refs_min10 %>% 
  arrange(desc(total_cards_per_game)) %>% 
  ggplot(aes(x = reorder(Referee, +total_cards_per_game), y = total_cards_per_game)) + # reorder arranges bar in desc order
  geom_bar(stat = "identity", color = "yellow", fill = "red")  +
  coord_flip() 

# average number of yellows and reds given in a game

  # first reshape df from wide to long
strict_refs_long <- strict_refs_min10 %>% 
  pivot_longer(!Referee, names_to = "card_metric", values_to = "count")


  ggplot(aes)