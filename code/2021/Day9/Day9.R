# Day 9: Statistics
# 04/09/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: https://cran.r-project.org/web/packages/datasauRus/vignettes/Datasaurus.html

# Setup-----
library(tidyverse)
library(patchwork)

theme_set(theme_minimal(base_family = "Roboto Slab"))

# Read data
datasaurus <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

datasaurus <- datasaurus %>% 
  mutate(dataset = case_when(
    dataset == "away" ~ "Away", 
    dataset == "bullseye" ~ "Bullseye", 
    dataset == "circle" ~ "Circle", 
    dataset == "dino" ~ "Dino", 
    dataset == "dots" ~ "Dots", 
    dataset == "h_lines" ~ "Horizontal Lines",
    dataset == "high_lines" ~ "High Lines",
    dataset == "slant_down" ~ "Slant Down", 
    dataset == "slant_up" ~ "Slant Up",
    dataset == "star" ~ "Star",
    dataset == "v_lines" ~ "Vertical Lines",
    dataset == "wide_lines" ~ "Wide Lines",
    dataset == "x_shape" ~ "X Shape"
  ))

p1 <- datasaurus %>% 
  ggplot(aes(x = x, y = dataset, fill = dataset, group = dataset)) +
  geom_violin() +
  stat_summary(fun = mean,
               fun.min = function(y) mean(y) - sd(y), 
               fun.max = function(y) mean(y) + sd(y), 
               geom = "pointrange")  + 
  facet_wrap(~dataset, scales =  "free_y", ncol = 1) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank())

p2 <- datasaurus %>% 
  ggplot(aes(x = x, y = y, color = dataset)) +
  geom_point() +
  scale_y_continuous(position = "right") +
  facet_wrap(~dataset,ncol = 1) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid.minor = element_blank())

p1 + p2 +
  plot_annotation(
    title = "Plot Your Data!",
    subtitle = "The datasaurus data package includes 13 sets of x-y data. The following statistics are (almost) the same for each set 
of data: mean of x, mean of y, standard deviation of x, standard deviation of y, and Pearson correlation between x and y. 
Plotting the data reveals a very different picture.",
    caption = "Created by @kllycttn | Data from Albert Cairo and the datasaurus package",
    theme = theme(plot.title = element_text(face = "bold", size = 35, hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5))
  ) 

ggsave(here::here("code", "Day9", "day9.png"), height = 25, width = 10, units = "in")

