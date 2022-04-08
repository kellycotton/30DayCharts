# Day 8: Mountains
# 04/08/2021
# https://github.com/Z3tt/30DayChartChallenge_2022
# Data: Colorado Fourteeners Mike Wehinger via Kaggle
# https://www.kaggle.com/datasets/mikeshout/14erpeaks

# Setup-----
library(tidyverse)
library(ggridges)
library(ggtext)

set.seed(123)
theme_set(theme_minimal(base_family = "Lato"))

# Color palette
pal <- c("#40312C", "#5D4840", "#727460", "#87A07F", "#B2C2AD", "#C7D2C4")

# Subtitle with specific colors
sub = "\"Fourteener\" refers to mountain peaks with an elevation of at least 14,000 feet (4,267 meters). 
Of the 96 found in the US, <br> Colorado has the most fourteeners of any state with 53. The tallest 
Colorado mountain is Mount Elbert, in the <span style = 'color:#C7D2C4;'><strong>Sawatch Range</strong></span>."

# Data
mountains <- read_csv(here::here("code", "2022", "Day8", "14er.csv"))
mountains <- janitor::clean_names(mountains)

# Tallest
mountains %>% 
  slice_max(order_by = elevation_ft, n = 1)

mountains %>% 
  ggplot(aes(x = elevation_ft, y = mountain_range, color = mountain_range, fill = mountain_range)) +
  geom_density_ridges(jittered_points = TRUE, 
                      position = position_points_jitter(width = 0.02, yoffset = -.25, seed = 1),
                      alpha = 0.6) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_discrete(labels = scales::wrap_format(10)) +
  xlab("Elevation (ft)") +
  labs(title = "Colorado Fourteeners", 
       subtitle = sub, 
       caption = "Created by @kllycttn | Data from Wikipedia via Mike Wehinger") +
  theme(
    panel.grid.minor.x = element_blank(), 
    legend.position = "none",
    axis.title.y = element_blank(),
    plot.subtitle = element_markdown(size = 12),
    plot.title = element_text(family = "OldSansBlack", size = 35),
    axis.text.y = element_text(face = "bold")
  )

ggsave(here::here("code","2022", "Day8", "day8.png"), width = 10, height = 8, unit = "in", bg = "white")

  