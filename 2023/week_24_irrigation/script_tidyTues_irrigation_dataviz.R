# Plotting irrigation data

library(tidyverse) # includes ggplot lib
library(paletteer)

interviews_plotting <- read_csv("data_output/interviews_plotting.csv")

### SCATTERPLOTS ####

# assign plot to a variable 
interviews_plot <- interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = num_items_owned))

# draw the plot as a dot plot
interviews_plot +
  geom_point() # there seem to be less dots than the number of obs in the dataset. 
  # consider changing the transparency of the points by adjusting the alpha argument 

interviews_plot +
  geom_point(alpha= 0.3) # this didn't really address the issue, let's try jiggering the points

interviews_plot +
  geom_jitter() # this seemed to make the individual points easier to distinguish. 

# we can specify the amount of random motion in the jitter using the width and height arguments

interviews_plot +
  geom_jitter(alpha = 0.5, # adjust transparency
              #color = "red",
              width = 0.2, # adjust horizontal jitter
              height = 0.2,) + # adjust vertical jitter 
  scale_color_paletteer_d("nord::frost")


# change the color of the points based on the village
interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = num_items_owned)) +
  geom_jitter(aes(color = village), alpha = 0.7, width = 0.2, height = 0.2)


# use geom_count to make each point representative of the number of observations of that type

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = num_items_owned, color = village)) +
  geom_count()

#  create a scatter plot of rooms by village with the respondent_wall_type showing in different colours

interviews_plotting %>% 
  ggplot(aes(x = village, y = rooms, color = respondent_wall_type )) +
  geom_jitter(alpha = 0.7, width = 0.3, height = 0.3)

### BOXPLOTS ##### 

interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) +
  geom_jitter(alpha = 0.3,
             color = "tomato",
             height = 0.2,
             width = 0.2) +
  geom_boxplot(alpha = 0) 

# replace boxplot with a violin plot. Boxplots are useful, but can hide the shape of the distribution

interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) +
  geom_violin( ) +
  geom_jitter(alpha = 0.3,
              color = "tomato",
              height = 0.2,
              width = 0.2)

# plot distribution of room number with months_lack_food

interviews_plotting %>%
  ggplot(aes(x = respondent_wall_type, y = num_items_owned)) +
  geom_violin() +
  geom_jitter(alpha = 0.3,
              color = "tomato",
              height = 0.2,
              width = 0.2)

# plot distribution of room number with months_lack_food

interviews_plotting %>%
  ggplot(aes(x = respondent_wall_type, y = liv_count)) +
  geom_violin() + # violin plot
  geom_jitter(alpha = 0.3,
              color = "tomato",
              height = 0.2,
              width = 0.2)

interviews_plotting %>%
  ggplot(aes(x = respondent_wall_type, y = liv_count)) +
  geom_boxplot() + # box plot
  geom_jitter(aes(color = memb_assoc),
              alpha = 0.5,
              height = 0.2,
              width = 0.2)

### BARPLOTS ####


interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type)) +
  geom_bar()

# stacked bar chart
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type)) +
  geom_bar(aes(fill = village))

# dodge bar chart
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type)) +
  geom_bar(aes(fill = village), position = "dodge")


# Plot proportion of homes in each village for each wall type

percent_wall_type <- interviews_plotting %>% 
  filter(respondent_wall_type != "cement") %>% 
  count(village, respondent_wall_type) %>% 
  group_by(village) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  ungroup()
  
percent_wall_type %>% 
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis(discrete = TRUE)
  #scale_color_gradientn(colors = rev(soil_palette("redox2",5))) 



# Create bar chart showing the proportion of respondents in each village who are
# or are not part of an irrigation association (memb_assoc)

# create a dataframe and add a column that shows the percent in each village in a irrigation association
percent_memb_assoc <- interviews_plotting %>% 
  # remove records with NA in member association
  filter(!is.na(memb_assoc)) %>% 
  count(village, memb_assoc) %>% 
  group_by(village) %>% 
  mutate(percent_mem_assoc = n / sum(n) * 100) %>% 
  ungroup()

percent_memb_assoc %>% 
  ggplot(aes(x = village, y = percent_mem_assoc, fill = memb_assoc)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of households in each village that are in an irrigation association",
       fill = "Irrigation Association Member?",
       x = "Village",
       y = "Proportion of members in irrigation association") +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 45,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 12)) +
  scale_fill_viridis(discrete = TRUE)

percent_wall_type %>%
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of wall type by village",
       fill = "Type of Wall in Home",
       x = "Village",
       y = "Percent")

percent_wall_type %>%
  ggplot(aes(x = respondent_wall_type, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Proportion of wall type by village",
       x="Wall Type",
       y="Percent") +
  facet_wrap(~ village) +
  theme_bw() +
  theme(panel.grid = element_blank())


## Proportion of households who owned a particular item

# Create a dataframe with new column
percent_items <- interviews_plotting %>%
  group_by(village) %>%
  select(!c(no_listed_items,car)) %>% 
  summarize(across(bicycle:computer, ~ sum(.x) / n() * 100)) %>%
  pivot_longer(bicycle:computer, names_to = "items", values_to = "percent")

percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village who owned each item",
       x = "Village",
       y = "Percent of Respondents") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 45,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))


# save the plot
my_plot <- percent_items %>%
  ggplot(aes(x = village, y = percent, fill = village)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste(format(percent, digits = 1), "%")), size = 3.5, color = "grey20", fontface = "bold", vjust = -0.15) +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village \n who owned each item",
      subtitle = "Data from SAFI (Studying African Farmer-Led Irrigation) Surveys in Tanzania and Mozambique.",
       x = "Village",
       y = "Percent of Respondents", 
      caption = "The survey data was collected through interviews conducted between November 2016 and June 2017") +
  theme(axis.text.x = element_blank(),
        strip.text.x = element_text(size=12, face="italic"),
        axis.text.y = element_text(color = "grey20", size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 14),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(hjust = 0.5, size = 12)) + 
  scale_y_continuous(limits = c(0,85)) +
  scale_fill_brewer(type = "qual", palette = "Accent")

my_plot


ggsave("fig_output/items_owned.png", my_plot, width = 15, height = 10)






library(ggplot2)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_paletteer_d("nord::aurora")
