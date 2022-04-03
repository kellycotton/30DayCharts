# Day 3: Historical
# 04/02/2021
# https://github.com/30DayChartChallenge/Edition2022
# Data from WhalingHistory.org, Census of Marine Life
# https://whalinghistory.org/av/logs/coml/
# via Data Is Plural
# https://docs.google.com/spreadsheets/d/1wZhPLMCHKJvwOkP4juclhjFgqIY8fQFMemwKL2c64vk/edit#gid=0


# Setup-----
library(tidyverse)
library(lubridate)
library(ggtext)

# Read data
whales <- read_tsv(here::here("code", "2022", "Day3", "whales.txt"))

pal <- c("#002147", "#586994", "#78C0E0", "#A7CCED")
theme_set(theme_minimal(base_family = "Neutra Text"))

# Subtitle with specific colors
sub_whale = "How often did American whaling voyages actually encounter whales? The data below, courtesy of WhalingHistory.org, 
show the number of <br> encounters (size of circle) each year. Four different types of encounters are documented: 
<span style = 'color:#A7CCED;'><strong>strikes</strong></span>, 
<span style = 'color:#78C0E0;'><strong>sightings</strong></span>, 
<span style = 'color:#586994;'><strong>lowered boats</strong></span>, 
and <span style = 'color:#002147;'><strong>deaths</strong></span>."

whales$decade <- parse_date_time(whales$Year,"y")
whales$decade <- floor_date(whales$decade, years(10))
whales$decade <- gsub("-.*","", whales$decade)

whales$century <- parse_date_time(whales$Year,"y")
whales$century <- floor_date(whales$century, years(100))
whales$century <- gsub("-.*","", whales$century)

whales_events <- whales %>% 
  group_by(century, decade, Enc) %>% 
  filter(!is.na(decade)) %>% 
  count()

ggplot(whales_events, aes(x = decade, y = Enc, size = n, color = Enc)) + 
  geom_point() +
  facet_wrap(~century, scales = "free_x") +
  scale_color_manual(values = pal) +
  scale_y_discrete(expand = c(0.05, 0)) +
  labs(title = "Whale Encounters, 1760 - 1920", 
       subtitle = sub_whale,
       caption = "Created by @kllycttn | Data from WhalingHistory.Org") +
  theme(
    legend.position = "none",
    plot.title = element_text(family = "Noto Serif", face = "bold", size = 29, hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5, size = 13),
    axis.title = element_blank()
  )

ggsave(here::here("code","2022", "Day3", "day3.png"), width = 11, height = 8, unit = "in", bg = "white")

  