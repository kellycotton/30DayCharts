# Day 10: Experimental
# 04/10/2022
# https://github.com/Z3tt/30DayChartChallenge_2022
# Data: the Center for Near-Earth Object Studies, NASA, via Shruti Mehta
# https://www.kaggle.com/datasets/shrutimehta/nasa-asteroids-classification

# Setup----
library(tidyverse)
library(ggdark)
library(ggtext)

pal <- c("#454c92", "#bbb4e9")

data <- read_csv(here::here("code", "2022", "Day10", "nasa.csv"))
data <- janitor::clean_names(data)

data_filter <- data %>% filter(est_dia_in_km_max < 20)

ggplot(data_filter, aes(y = est_dia_in_km_max, x = relative_velocity_km_per_hr, color = hazardous)) +
  geom_point(alpha = 0.7) +
  geom_rug() +
  scale_color_manual(values = pal) +
  ylab("Estimated Diameter Max (km)") + xlab("Relative Velocity (km/hr)") +
  labs(
    title = "*ASTEROIDS*",
    subtitle = "The plot below shows the relative velocity and diameter of 4,686 \"Near-Earth Objects\", 
    <br>including whether they are considered <span style = 'color:#bbb4e9;'><strong>hazardous</strong></span> 
    or <span style = 'color:#454c92;'><strong>non-hazardous</strong></span>. Missing from this plot 
    <br>is one particularly large asteroid, estimated to be 34 km. Don't worry, it was non-hazardous!",
    caption = "Created by @kllycttn | Data from the Center for Near-Earth Object Studies, NASA"
  ) +
  dark_theme_minimal() +
  coord_polar() +
  theme(
    plot.title = element_markdown(family = "Kanit", face = "bold", size = 40, hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5, family = "Libre Franklin", size = 15),
    axis.title = element_text(family = "Libre Franklin", size = 9),
    axis.text = element_text(family = "Libre Franklin", size = 8),
    legend.position = "none"
  )
  
ggsave(here::here("code","2022", "Day10", "day10.png"), width = 11, height = 11, unit = "in", bg = "black")

