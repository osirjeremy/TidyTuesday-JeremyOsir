
# Set up folder
dir.create("2024-06-04_cheeses")
setwd("~/Desktop/D_Sci/tidyTuesdays/2024/2024-06-04_cheeses/")

# Load data
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
cheeses <- tuesdata$cheeses

write.csv(cheeses, file = "./cheese_data.csv", row.names = FALSE)


# Research Questions: 248 cheeses have listed fat content.
# Is there a relationship between fat content and cheese type? 
# What about texture, flavor, or aroma?

