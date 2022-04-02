# Day 2: Pictogram
# 04/02/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: https://www.kaggle.com/jeffreybraun/shake-shack-restaurant-locations


# Setup-----
library(tidyverse)
library(waffle)
library(ggtext)

# Read data
food <- read_csv(here::here("code", "Day2", "shakeshack.csv"))

# Color palette
pal <- RColorBrewer::brewer.pal(8, "Dark2")[c(1:4, 7:8)]

# Subtitle with specific colors
sub_shack = "100 Shake Shack locations can be found in <span style = 'color:#1B9E77;'>New York</span> (35), 
<span style = 'color:#D95F02;'>California</span> (21), 
<span style = 'color:#7570B3;'>Texas</span> (18), 
<span style = 'color:#E7298A;'>Florida</span> (15),
and <span style = 'color:#A6761D;'>New Jersey</span> (11), 
more than <span style = 'color:#666666;'>all other</span> states combined (93)."

# Organize data and create plot
food %>% 
  group_by(state) %>% 
  count(sort = TRUE) %>% 
  mutate(label = ifelse(n < 10, "All Others", state),
         label = factor(label, levels = c("New York", "California", "Texas", "Florida", "New Jersey", "All Others"))) %>% 
  ggplot(aes(label = label, values = n, color = label)) +
  geom_pictogram() +
  scale_label_pictogram(name = NULL,
                        values = c("hamburger", "hamburger", "hamburger", "hamburger", "hamburger", "hamburger")) +
  scale_color_manual(name = NULL,
                     values = pal) +
  theme_void() +
  coord_equal() +
  labs(title = "Where can you get a ShackBurger?",
       subtitle = sub_shack,
       caption = "Created by @kllycttn | Data from Shake Shack, Kaggle") +
  theme(plot.title = element_text(family = "Neutra Text", face = "bold", size = 30, hjust = 0.5),
        plot.subtitle = element_markdown(family = "Neutra Text", hjust = 0.5, size = 9),
        legend.position = "bottom",
        )
  
ggsave(here::here("code", "Day2", "day2.png"))

