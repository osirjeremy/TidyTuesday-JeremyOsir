library(tidyverse)
library(here)
library(janitor)
library(readr)
library(ggplot2)
library(ggthemr)
library(gridExtra)
library(scales)


# Set the working directory
# setwd("/Users/jeremyosir/Desktop/D_Sci/tidyTuesdays/2024/2024-01-09_hockey_birth_months")
working_dir <- here::here("2024", "2024-01-09_NHL_birth_months", "data")


# Import data directly from GitHub

canada_births_1991_2022 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/canada_births_1991_2022.csv')
nhl_player_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv')
nhl_rosters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv')
nhl_teams <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_teams.csv')


#### CANADA NATIONAL DATA: What are the Percentage of Births Occuring in each month ####

# Combine 'year' and 'month' to create a Date column
canada_births_1991_2022$date <- as.Date(paste(canada_births_1991_2022$year, canada_births_1991_2022$month, "1", sep = "-"))

# Aggregate the data to get the total number of births for each month
monthly_births_canada <- aggregate(births ~ month, data = canada_births_1991_2022, sum)

# Calculate the percentage of total births for each month
monthly_births_canada$percentage <- (monthly_births_canada$births / sum(monthly_births_canada$births)) * 100

# Create a bar plot using ggplot2 with percentage labels, rotated x-axis labels, and reduced space
# ggplot(monthly_births_canada, aes(x = factor(month, levels = 1:12, labels = month.name), y = percentage, fill = factor(month))) +
#   geom_bar(stat = "identity", color = "black") +
#   geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +
#   labs(title = "Percentage of Births Occuring in Each Month",
#        subtitle = "Canada Birth Data, 1991 - 2022",
#        caption = "Source: Statistics Canada, Canadian Vital Statistics - Birth database (CVSB).",
#        x = "Month",
#        y = "Percentage of Births") +
#   scale_fill_brewer(palette = "Set3") +  # You can choose a different color palette
#   ggthemes::theme_fivethirtyeight() +
#   theme(axis.text.x = element_text(angle = 0, vjust = 1.5, hjust = 1, margin = margin(b = -10), size = 10, face = "bold"),  # Reduce space with margin
#         axis.text.y = element_blank()) +  # Remove y-axis labels # Reduce space with margin
#   guides(fill = FALSE)

# Create a bar plot using ggplot2 with percentage labels, rotated x-axis labels, reduced space, and 1 decimal place
plot_canada <- ggplot(monthly_births_canada, aes(x = factor(month, levels = 1:12, labels = month.name), y = percentage, fill = factor(month))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +
  labs(title = "Percentage of Canadians Born in each Month",
       subtitle = "Canada Birth Data, 1991 - 2022 ",
       caption = "Source: Statistics Canada, Canadian Vital Statistics - Birth database (CVSB).",
       x = "Month",
       y = "Percentage of Births") +
  scale_fill_brewer(palette = "Paired") +  # You can choose a different color palette
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +  # Abbreviate month names
  ggthemes::theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 0, vjust = 10, hjust = 0.5, margin = margin(b = -10), size = 10, face = "bold"),  # Reduce space with margin
        axis.text.y = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 14, face = "italic")) +  # Remove y-axis labels
  guides(fill = FALSE)

plot_canada

#### NHL Player Birth Data ####
monthly_births_nhl <- nhl_player_births %>% 
  rename(month = birth_month) %>% 
  filter(birth_country == "CAN") %>% 
  group_by(month) %>% 
  summarize(births = n())

# Calculate the percentage of total births for each month
monthly_births_nhl$percentage <- (monthly_births_nhl$births / sum(monthly_births_nhl$births)) * 100

# Create a bar plot using ggplot2 with percentage labels, rotated x-axis labels, reduced space, 1 decimal place, and ggthemr theme
plot_nhl <- ggplot(monthly_births_nhl, aes(x = factor(month, levels = 1:12, labels = month), y = percentage, fill = factor(month))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +
  labs(title = "Percentage of NHL Players Born in Each Month",
       subtitle = "NHL Data, 1885 - 2005",
       x = "Birth Month",
       y = "Percentage of Births") +
  scale_fill_brewer(palette = "Paired") +  # You can choose a different color palette
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +  # Abbreviate month names
  ggthemes::theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 0, vjust = 10, hjust = 0.5, margin = margin(b = -10), size = 10, face = "bold"),  # Reduce space with margin
        axis.text.y = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 14, face = "italic")) +  # Remove y-axis labels
  guides(fill = FALSE)

plot_nhl

grid.arrange(plot_canada, plot_nhl, ncol = 2)

#### Combine Canada and NHL datasets so that we can visualize the monthly birth percentages, side-by-side ####

# Add a new column to each data frame to indicate the dataset
monthly_births_canada$dataset <- "Canada"
monthly_births_nhl$dataset <- "NHL"

# Combine the data frames

combined_births <- bind_rows(monthly_births_canada, monthly_births_nhl)

# Define the abbreviated month names
abbreviated_month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Plot the combined data
nhl_birth_plot <- ggplot(combined_births, aes(x = month, y = percentage, fill = dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", percentage)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5, fontface = 'bold') +
  labs(title = "NHL Players are more likely to be born in the first quarter of the year ",
       x = "Birth Month",
       y = "Percent") +
  scale_y_continuous(labels = percent_format(scale = 1),
                     limits = c(0, max(combined_births$percentage) + 0.1)) +
  scale_x_continuous(breaks = 1:12, labels = abbreviated_month_names) +
  ggthemes::theme_fivethirtyeight() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 14, face = "italic"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 5, hjust = 0.5, margin = margin(b = -10), size = 10, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(face = "bold", margin = margin(r = -20)),
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        legend.position = "bottom",  # Move legend to the bottom
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold")) + # Add margin to y-axis title
  scale_fill_manual(name = "Legend", 
                    values = c("Canada" = "blue", "NHL" = "orange"),
                    labels = c("% of Canadians born in each month", "% of NHL Players born in each month"))
nhl_birth_plot

# Save the plot as an image (adjust width and height as needed)
ggsave("barplot_nhl_births.png", nhl_birth_plot, width = 10, height = 6, units = "in", dpi = 300)

