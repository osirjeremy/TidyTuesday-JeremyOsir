# Get the Data

# Read in with tidytuesdayR package 
install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2023-04-04')
#tuesdata <- tidytuesdayR::tt_load(2023, week = 14)

soccer <- tuesdata$soccer
