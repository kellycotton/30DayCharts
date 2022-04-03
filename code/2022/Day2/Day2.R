# Day 2: Pictogram
# 04/02/2021
# https://github.com/30DayChartChallenge/Edition2022


# Setup-----
library(tidyverse)
library(waffle)



# Read data
breed_traits <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
breed_rank_all <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_traits <- janitor::clean_names(breed_traits)
breed_rank_all <- janitor::clean_names(breed_rank_all)

breed_traits <- breed_traits %>% 
  mutate(breed = gsub("[[:punct:]]","" , breed)) # remove punctuation

breed_rank_all <- breed_rank_all %>% 
  mutate(breed = gsub("[[:punct:]]","" , breed)) # remove punctuation

breed_combined <- left_join(breed_traits, breed_rank_all, by = "breed")

top_breed <- breed_traits %>% 
  filter(breed %in% top_3)
