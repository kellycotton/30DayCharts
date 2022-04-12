# Day 12: The Economist
# 04/12/2022
# https://github.com/Z3tt/30DayChartChallenge_2022
# Data: Our World in Data
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-04-12/readme.md

# Setup-----
library(tidyverse)
library(ggthemes)
library(lubridate)

# Data
indoor_pollution <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')

indoor_pollution <- janitor::clean_names(indoor_pollution) %>% rename(deaths = starts_with("deaths"))

# Summarize yearly
indoor_reduced <- indoor_pollution %>% 
  filter(!is.na(code)) %>% 
  group_by(year, entity) %>% 
  summarize(mean_deaths_year = mean(deaths))

# Add decade to years
indoor_reduced$decade <- parse_date_time(indoor_reduced$year,"y")
indoor_reduced$decade <- floor_date(indoor_reduced$decade, years(10))
indoor_reduced$decade <- gsub("-.*","", indoor_reduced$decade)

# Summarize decades
indoor_decade <- indoor_reduced %>% 
  group_by(entity, decade) %>% 
  summarize(mean_deaths_deade = mean(mean_deaths_year))

# Plot
ggplot(indoor_reduced, aes(x = mean_deaths_year, group = year)) + 
  geom_density(alpha = 0.5, color = "gray") +
  geom_density(data = indoor_decade, aes(x = mean_deaths_deade, color = decade), inherit.aes = FALSE,
               size = 1.2) +
  annotate(geom = "text", 
           label = "1990s",
           x = 1,
           y = 0.067,
           color = "#6592a7",
           fontface = "bold",
           family = "EconSansCndBol") + 
  annotate(geom = "text", 
           label = "2000s",
           x = 3,
           y = 0.09,
           color = "#014965",
           fontface = "bold",
           family = "EconSansCndBol") +
  annotate(geom = "text", 
           label = "2010s",
           x = 11,
           y = 0.025,
           color = "#069ce1",
           fontface = "bold",
           family = "EconSansCndBol") +
  scale_color_economist() +
  scale_y_continuous(position = "right") +
  xlab("% of Deaths per Year") +
  labs(
    title = "Global Deaths Attributed to Air Pollution",
    subtitle = "Share of deaths attributed to indoor air pollution per year",
    caption = "Created by @kllycttn | Data from Our World in Data"
  ) +
  theme_economist_white(gray_bg = FALSE) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_text(vjust = -.2,
                                    family = "EconSansCndReg"),
        axis.text = element_text(family = "EconSansCndReg"),
        plot.title = element_text(family = "EconSansCndBol"),
        plot.subtitle = element_text(family = "EconSansCndReg", hjust = 0),
        plot.caption = element_text(family = "EconSansCndReg", size = 7, hjust = 1)
        )

ggsave(here::here("code","2022", "Day12", "day12.png"), width = 10, height = 8, unit = "in", bg = "white")

    