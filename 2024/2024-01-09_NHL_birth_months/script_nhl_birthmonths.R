library(tidyverse)
library(here)
library(janitor)
library(readr)
library(ggplot2)
library(ggthemr)


# Set the working directory
# setwd("/Users/jeremyosir/Desktop/D_Sci/tidyTuesdays/2024/2024-01-09_hockey_birth_months")
working_dir <- here::here("2024", "2024-01-09_NHL_birth_months", "data")


# Import data directly from GitHub

canada_births_1991_2022 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/canada_births_1991_2022.csv')
nhl_player_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv')
nhl_rosters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv')
nhl_teams <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_teams.csv')


# CANADA: What are the Percentage of Births Occuring in each month
# Combine 'year' and 'month' to create a Date column
canada_births_1991_2022$date <- as.Date(paste(canada_births_1991_2022$year, canada_births_1991_2022$month, "1", sep = "-"))

# Aggregate the data to get the total number of births for each month
monthly_births <- aggregate(births ~ month, data = canada_births_1991_2022, sum)

# Calculate the percentage of total births for each month
monthly_births$percentage <- (monthly_births$births / sum(monthly_births$births)) * 100

# Create a bar plot using ggplot2 with percentage labels, rotated x-axis labels, and reduced space
ggplot(monthly_births, aes(x = factor(month, levels = 1:12, labels = month.name), y = percentage, fill = factor(month))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +
  labs(title = "Percentage of Births Occuring in Each Month",
       subtitle = "Canada Birth Data, 1991 - 2022",
       caption = "Source: Statistics Canada, Canadian Vital Statistics - Birth database (CVSB).",
       x = "Month",
       y = "Percentage of Births") +
  scale_fill_brewer(palette = "Set3") +  # You can choose a different color palette
  ggthemes::theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1.5, hjust = 1, margin = margin(b = -10), size = 10, face = "bold"),  # Reduce space with margin
        axis.text.y = element_blank()) +  # Remove y-axis labels # Reduce space with margin
  guides(fill = FALSE)

# Create a bar plot using ggplot2 with percentage labels, rotated x-axis labels, reduced space, and 1 decimal place
ggplot(monthly_births, aes(x = factor(month, levels = 1:12, labels = month.name), y = percentage, fill = factor(month))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +
  labs(title = "Percentage of Births Occuring in Each Month",
       subtitle = "Canada Birth Data, 1991 - 2022",
       caption = "Source: Statistics Canada, Canadian Vital Statistics - Birth database (CVSB).",
       x = "Month",
       y = "Percentage of Births") +
  scale_fill_brewer(palette = "Set3") +  # You can choose a different color palette
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +  # Abbreviate month names
  ggthemes::theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 0, vjust = 20, hjust = 0.5, margin = margin(b = -10), size = 10, face = "bold"),  # Reduce space with margin
        axis.text.y = element_blank(),
        plot.subtitle = element_text(size = 14)) +  # Remove y-axis labels
  guides(fill = FALSE)


