# Day 2: Pictogram
# 04/02/2021
# https://github.com/30DayChartChallenge/Edition2022
# Data: American Kennel Club c/o kkakey
# https://github.com/kkakey/dog_traits_AKC/blob/main/README.md


# Setup-----
library(tidyverse)
library(ggimage)

theme_set(theme_minimal(base_family = "Noto Serif"))


# Read data
breed_traits <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
breed_rank_all <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_traits <- janitor::clean_names(breed_traits)
breed_rank_all <- janitor::clean_names(breed_rank_all)

dog_image <- breed_rank_all %>% filter(breed == "Dachshunds") %>% pull(image)


breed_weiner <- breed_traits %>% 
  filter(breed == "Dachshunds") %>% 
  select(c(breed, affectionate_with_family, shedding_level, drooling_level, energy_level, barking_level)) %>% 
  pivot_longer(-breed,
               names_to = "characteristic", 
               values_to = "rating") %>% 
  mutate(four_rating = ifelse(rating == 5, 4, NA),
         three_rating = ifelse(rating > 3, 3, NA), 
         two_rating = ifelse(rating > 2, 2, NA), 
         one_rating = 1)


ggplot(breed_weiner, aes(x = rating, y = characteristic)) +
  geom_image(image = dog_image, size = 0.13) +
  geom_image(aes(x = four_rating), image = dog_image, size = 0.13) +
  geom_image(aes(x = three_rating), image = dog_image, size = 0.13) +
  geom_image(aes(x = two_rating), image = dog_image, size = 0.13) +
  geom_image(aes(x = one_rating), image = dog_image, size = 0.13) +
  scale_x_continuous(limits = c(0.75, 5)) +
  scale_y_discrete(labels = c("Affectionate", "Barking Level", "Drooling Level", "Energy Level", "Shedding Level")) +
  labs(title = "Dachshunds: hot dogs or cold dogs?",
       subtitle = "Is the infamous weiner dog a good dog for you? Ratings on some key considerations, courtesy of the American Kennel Club.",
       caption = "Created by @kllycttn | Data from the American Kennel Club") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        plot.title = element_text(family = "Titillium Web", face = "bold", size = 30, hjust = 0.5))

ggsave(here::here("code","2022", "Day2", "day2.png"), width = 11, height = 8, unit = "in", bg = "white")

