# Day 11: Circular
# 04/11/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: https://github.com/MuseumofModernArt/collection

# Setup-----
library(tidyverse)
library(ggridges)
library(patchwork)

theme_set(theme_minimal(base_family = "Merriweather"))

pal <- RColorBrewer::brewer.pal(n = 10, "Paired")  

# Read data
happy <- read_csv(here::here("code", "Day11", "world-happiness-report-2021.csv"))

happy <- happy %>% mutate(`Regional indicator` = factor(`Regional indicator`, 
                                                        levels = c("Central and Eastern Europe",
                                                                   "Western Europe",
                                                                   "Latin America and Caribbean",
                                                                   "North America and ANZ",
                                                                   "East Asia",
                                                                   "Southeast Asia",
                                                                   "South Asia",
                                                                   "Middle East and North Africa",
                                                                   "Sub-Saharan Africa",
                                                                   "Commonwealth of Independent States")))

p1 <- ggplot(happy, aes(x  = `Ladder score`, y = `Regional indicator`, fill = `Regional indicator`)) +
  geom_density_ridges() +
  coord_polar() +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  scale_fill_manual(values = pal,
                    labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 8, hjust = 0.5)
  ) +
  ggtitle("Ladder Score")

p2 <- ggplot(happy, aes(x  = `Logged GDP per capita`, y = `Regional indicator`, fill = `Regional indicator`)) +
  geom_density_ridges() +
  coord_polar() +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  scale_fill_manual(values = pal,
                    labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 8, hjust = 0.5)
  ) +
  ggtitle("GDP per capita")

p3 <- ggplot(happy, aes(x  = `Social support`, y = `Regional indicator`, fill = `Regional indicator`)) +
  geom_density_ridges() +
  coord_polar() +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  scale_fill_manual(values = pal,
                    labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 8, hjust = 0.5)
  ) +
  ggtitle("Social Support")


p4 <- ggplot(happy, aes(x  = `Healthy life expectancy`, y = `Regional indicator`, fill = `Regional indicator`)) +
  geom_density_ridges() +
  coord_polar() +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  scale_fill_manual(values = pal,
                    labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 8, hjust = 0.5)
  ) +
  ggtitle("Healthy Life Expectancy")


p5 <- ggplot(happy, aes(x  = `Freedom to make life choices`, y = `Regional indicator`, fill = `Regional indicator`)) +
  geom_density_ridges() +
  coord_polar() +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  scale_fill_manual(values = pal,
                    labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 8, hjust = 0.5)
  ) +
  ggtitle("Freedom to Make Life Choices")

p6 <- ggplot(happy, aes(x  = `Generosity`, y = `Regional indicator`, fill = `Regional indicator`)) +
  geom_density_ridges() +
  coord_polar() +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  scale_fill_manual(values = pal,
                    labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 8, hjust = 0.5)
  ) +
  ggtitle("Generosity")


p7 <- ggplot(happy, aes(x  = `Perceptions of corruption`, y = `Regional indicator`, fill = `Regional indicator`)) +
  geom_density_ridges() +
  coord_polar() +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  scale_fill_manual(values = pal,
                    labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 8, hjust = 0.5)
  ) +
  ggtitle("Perceptions of Corruption") 


p1/(p2+p3+p4+p5+p6+p7) +
  plot_layout(guides = "collect", widths = c(2, 1)) +
  plot_annotation(
    title = "The World Happiness Report",
    subtitle = "Ladder Score is obtained by asking respondents to think of a ladder, with the best possible life for them being a 10, and the worst possible life being a 0. 
They are then asked to rate their own current lives on that 0 to 10 scale. The six variables below, GDP per capita, Social Support, Healthy Life Expectancy, 
Freedom to Make Life Choices, Generosity, and Percpetions of Corruption have been found to be important in explaining differences in life evaluations.",
    caption = "Created by @kllycttn | Data from The World Happiness Report, Gallup World Poll"
  ) &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(.5, units = "cm"),
        legend.text = element_text(size = 7, face = "bold"),
        plot.subtitle = element_text(size = 10, family = "Source Sans Pro"),
        axis.text = element_text(size = 6),
        plot.title = element_text(face = "bold"))

ggsave(here::here("code", "Day11", "day11.png"), height = 10, width = 9, units = "in")

  
