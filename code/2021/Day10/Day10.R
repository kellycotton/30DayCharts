# Day 10: Abstract
# 04/10/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: https://github.com/MuseumofModernArt/collection

# Setup-----
library(tidyverse)
library(ggtext)

# Read data
art <- read_csv('https://media.githubusercontent.com/media/MuseumofModernArt/collection/master/Artworks.csv')

# Color palette, modeled on Hilma af Klint's "Series VIII. Picture of the Starting Point", 1920
pal <- c("#ee582f", "#e7923f", "#e6c45a", "#73ab8d", "#7096c3", "#345976", "#a98d9c", 
         "#c5a26b", "#262626", "#5c5967", "#9f9484")

# Format data for plotting
art_data <- art %>% 
  filter(Classification != "(not assigned)") %>% 
  mutate(Classification = case_when(
    str_detect(Classification, "Archive") ~ "Archives",
    Classification == "Multiple" ~ "Other",
    TRUE ~ Classification
  )) %>% 
  group_by(Classification) %>% 
  summarise(total = n()) %>% 
  mutate(Classification = ifelse(total < 1000, "Other", Classification)) %>% 
  ungroup() %>% 
  group_by(Classification) %>% 
  summarise(total = sum(total)) %>% 
  mutate(ymax = cumsum(total), 
         ymin = lag(ymax, n = 1, default = 0)) %>% 
  mutate(Classification = factor(Classification, levels = c("Architecture", "Archives", "Design",
                                                               "Drawing", "Illustrated Book", "Painting",
                                                               "Photograph", "Print", "Sculpture", "Video",
                                                               "Other")))

ggplot(art_data) +
  geom_rect(aes(xmin = 0, xmax = 11, ymin = ymin, ymax = ymax, fill = Classification),
            color = "#acaeb0",
            size = .1) +
  coord_polar() +
  scale_fill_manual(name = NULL,
                    values = pal) +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  theme_void() +
  labs(
    title = "The MoMA Collection",
    subtitle = "What is most common type of art at the Museum of Modern Art? 
<br>**33,025 photographs**. The design below is modeled after the 
<br>abstract artist Hilma af Klint's *Series VIII. Picture of the Starting 
<br>Point*, 1920.",
    caption = "Created by @kllycttn | Data from the Museum of Modern Art "
  ) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "#dfd9cd", color = "#dfd9cd"),
    panel.grid = element_line(color = "#dfd9cd"),
    panel.background = element_rect(fill = "#dfd9cd", color = "#dfd9cd"),
    plot.title = element_text(family = "Franklin Gothic Condensed", size = 30, hjust = 0.5),
    plot.subtitle = element_markdown(family = "Libre Franklin", hjust = 0.5),
    legend.text = element_text(family = "Libre Franklin", size = 6, face = "bold"),
    legend.box.margin = margin(0,0,15,0)
  ) 

ggsave(here::here("code", "Day10", "day10.png"), height = 7, width = 5.05, units = "in")

