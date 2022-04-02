# Day 2: Part-to-Whole comparison
# 04/02/2021
# https://github.com/30DayChartChallenge/Edition2022


# Setup-----
library(tidyverse)
library(waffle)



# Read data
babynames <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')


data_2017 <- babynames %>% 
  filter(year == 2017) %>% 
  slice_max(order_by = n, n = 100)
