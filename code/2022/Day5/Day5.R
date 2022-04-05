# Day 5: Slopes
# 04/05/2021
# https://github.com/30DayChartChallenge/Edition2022


# Setup-----
library(tidyverse)
library(lubridate)
library(ggtext)

theme_set(theme_minimal(base_family = "Assistant"))

# Read data
news_orgs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

# make years into decades
news_orgs$decade <- parse_date_time(news_orgs$year_founded,"y")
news_orgs$decade <- floor_date(news_orgs$decade, years(10))
news_orgs$decade <- gsub("-.*","", news_orgs$decade)

# Find publications from 2000 - 2019 
years <- news_orgs %>% 
  filter(!is.na(year_founded)) %>% 
  filter(year_founded >= 2000 & year_founded < 2020)

years %>% 
  group_by(decade) %>% 
  count()

pubs <- years %>% 
  pull(publication_name)

# Split coverage topics, count per decade
coverage <- news_orgs %>% 
  filter(!is.na(coverage_topics)) %>% 
  filter(publication_name %in% pubs) %>% 
  mutate(topic = strsplit(as.character(coverage_topics), ", ")) %>% # split the topics in each cell
  unnest(topic) %>%
  mutate(topic = gsub("[[:punct:]]","" , topic)) %>% # remove punctuation 
  group_by(topic, decade) %>% 
  count() %>% 
  mutate(color = ifelse(topic == "Government", "#F97068", ifelse(topic == "Business", "#777DA7", "#343F3E")),
         alpha = ifelse(topic %in% c("Government", "Business"), 1, 0.75))

# Plot----
ggplot(coverage, aes(x = decade, y = n, color = color, group = topic, alpha = alpha)) +
  geom_line() +
  scale_color_identity() +
  scale_x_discrete(expand = c(0.05,0), labels = c("2000 - 2009", "2010 - 2019")) +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*1)) +
  annotate("richtext",
           x = 1.35,
           y = 88,
           label = "Between 2000 - 2009, *50* publications were founded that covered <br><span style = 'color:#F97068;'><strong>Government</strong></span>. In the next decade, 105 were founded.",
           fill = NA,
           label.color = NA,
           family = "Assistant") +
  annotate("richtext",
           x = 1.72,
           y = 60,
           label = "Another fast-growing area was <span style = 'color:#777DA7;'><strong>Business</strong></span>, with <br>31 founded in the 2000s and 95 in the 2010s.",
           fill = NA,
           label.color = NA, 
           family = "Assistant") +
  labs(
    title = "How has local news changed?",
    subtitle = "In the US and Canada, 685 digitally focused, local news organizations were founded between 2000 and 2019.",
    caption = "Created by @kllycttn | Data from the Project Oasis"
  ) +
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(family = "Newsreader", face = "bold", size = 25),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave(here::here("code","2022", "Day5", "day5.png"), width = 8, height = 11, unit = "in", bg = "white")
