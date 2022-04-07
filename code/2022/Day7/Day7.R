# Day 7: Physical
# 04/07/2022
# https://github.com/Z3tt/30DayChartChallenge_2022
# Data: International Trail Running Association via Benjamin Nowak 
# https://github.com/BjnNowak/UltraTrailRunning

# Setup-----
library(tidyverse)
library(ggtext)

theme_set(theme_minimal(base_family = "Source Sans Pro"))

race <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv') %>% 
  filter(distance != 0) 

median_race <- median(race$distance)

# Set arrow coordinates
arrows <- 
  tibble(
    x1 = c(40, 170, 150), 
    x2 = c(33, 180, 160),
    y1 = c(.02, -.01, .185),
    y2 = c(.001, -.001, .18)
  )

race %>% 
  ggplot(aes(x = distance)) +
  geom_density() +
  geom_segment(x = median_race, y = 0, xend=median_race, yend=Inf,
               linetype = "longdash", alpha = 0.01) +
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_x_continuous(limits = c(0, 200)) +
  annotate(geom = "text",
           label = "The shortest race is Sparnatrail, 33.3 km", 
           x = 61,
           y = .02, 
           family = "Source Sans Pro", 
           size = 3) +
  annotate(geom = "text",
           label = "The longest race is Ultratour des 4 Massif, 179.1 km", 
           x = 143,
           y = -.008, 
           family = "Source Sans Pro",
           size = 3) +
  annotate(geom = "text",
           label = "The median race distance is 161.8 km", 
           x = 142,
           y = .19, 
           family = "Source Sans Pro",
           size = 3) +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", 
    inherit.aes = FALSE) +  
  annotate(geom = "text", 
           label = "Ultra Trail Running: How long are the longest races? ",
           x = 85,
           y = .11,
           size = 7,
           family = "Noto Serif", 
           fontface = "bold") +
  annotate(geom = "richtext", 
           label = "The graph below shows the distribution of trail running race distances 
           <br>from 2012 - 2021, collected by the International Trail Running Association.",
           x = 80,
           y = .097,
           size = 4,
           family = "Source Sans Pro",
           fill = NA,
           label.color = NA) +
  xlab("Race Distance") +
  labs(
    caption = "Created by @kllycttn | Data from the International Trail Running Association"
  ) +
  theme(
    panel.grid = element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.y = element_blank()
  )
ggsave(here::here("code","2022", "Day7", "day7.png"), width = 11, height = 8, unit = "in", bg = "white")
  