# Set up file directory
# dir.create("data")
# dir.create("data_output")
# dir.create("fig_output")

# load libraries
library(tidyverse)
library(here)
library(lubridate)

# download source file with irrigation data
interviews <-  read_csv(here("data","SAFI_clean.csv"), na = "NULL")

# Inspect data frames
view(interviews)
skimr::skim(interviews)

## create a vector from the data frame column "memb_assoc"
memb_assoc <- interviews$memb_assoc

## replace the missing data with "undetermined"
memb_assoc[is.na(memb_assoc)] <- "undetermined"

## convert it into a factor
memb_assoc <- as.factor(memb_assoc)
memb_assoc

#Plot the vector
plot(memb_assoc)

# Formating dates - convert interview date into three different cols for day,moth, year

# str(int) # shows the structure of the tibble

# examine the structure of the interview_date column
dates <- interviews$interview_date
str(dates)

# create new columns in our tibble to show the day, month and year
interviews$day <- day(dates)
interviews$month <-  month(dates)
interviews$year <-  year(dates)


# Exercise - create a tibble that contains only the village column and a new col "total_meals"
# total meals = total meals served in hh per day on avg, filter to only show rows with meals > 20

interviews_dplyr_ex <- interviews %>% 
  mutate(total_meals = no_membrs * no_meals) %>% 
  filter(., total_meals >20) %>% 
  select(., village, total_meals)
interviews_dplyr_ex


# Split-apply-combine data analysis and the summarize() function
interviews %>% 
  filter(., !is.na(memb_assoc)) %>% 
  group_by(., village, memb_assoc) %>% 
  summarize(., mean_no_meals = mean(no_meals),
            mean_members = mean(no_membrs)) %>% 
  arrange(., desc(mean_no_meals))

# Counting
interviews %>% 
  count(village, sort = TRUE)

# How many households in the survey have an average of 2 meals/day vs 3 meals
interviews %>% 
 # group_by(., no_meals) %>% 
  count(no_meals)

# Use group_by() and summarize() to find the mean, min, and max number of household members for each village

interviews %>% 
  group_by(., village) %>% 
  summarise(mean_hh = mean(no_membrs),
            min_hh = min(no_membrs),
            max_hh = max(no_membrs),
            no_obs = n())

# What was the largest household interviewed in each month?
interviews %>% 
  group_by(year, month) %>% 
  summarize(largest_hh = max(no_membrs))

# Pivot wider to create columns indicating the wall construction material

interviews_wide <- interviews %>% 
  mutate(., wall_type_logical = TRUE) %>% 
  pivot_wider(., names_from = respondent_wall_type, 
              values_from = wall_type_logical,
              values_fill = list(wall_type_logical = FALSE))
interviews_wide
  
# Pivot longer to create a single column for wall construction material

interviews_long <- interviews_wide %>% 
  pivot_longer(.,cols = c("muddaub", "cement", "sunbricks", "burntbricks"),
               names_to = "respondent_wall_type",
               values_to = "wall_type_logical") %>% 
  filter(., wall_type_logical) %>% 
  select(., -wall_type_logical)
interviews_long  

# Applying pivot wider to clean data
