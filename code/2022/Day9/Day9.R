# Day 9: Statistics
# 04/09/2021
# https://github.com/Z3tt/30DayChartChallenge_2022
# Data: US Census
# https://data.census.gov/cedsci/table?q=New%20York%20city,%20New%20York&y=2020&tid=ACSST5Y2020.S0801


# Setup-----
library(tidyverse)
library(ggtext)

# Build theme----
theme_plot <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#FAFEFF", color = "#FAFEFF"),
          panel.background = element_rect(fill = "#FAFEFF", color = "#FAFEFF"),
          text = element_text(color = "#36343B", family = "Helvetica"), 
          plot.title = element_text(color = "#36343B", hjust = 0.5, size = 22, family = "Helvetica", face = "bold"),
          plot.subtitle = element_markdown(hjust = 0.5, color = "#36343B", size = 9, family = "Helvetica", vjust = -0.5),
          plot.caption = element_text(color = "#36343B", size = 6, hjust = 1, family = "Helvetica"),
          axis.title = element_blank(),
          axis.text = element_text(color = "#36343B", size = 7),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none")
}

pal <- c("#0039A6", "#EE352E")

# Data
commute <- read_csv(here::here("code", "2022", "Day9", "nyc_commute.csv"), skip = 2)
commute <- janitor::clean_names(commute) %>% rename_with(~str_remove(., 'new_york_city_new_york_'))

commute_travel <- commute[(41:49), ] %>% 
  select(!contains("margin_of_error")) %>% 
  mutate(total_estimate = as.numeric(gsub("%$","", total_estimate)),
         male_estimate = as.numeric(gsub("%$","", male_estimate)),
         female_estimate = as.numeric(gsub("%$","", female_estimate)),
         label_grouping = as.factor(str_trim(label_grouping))) %>% 
  pivot_longer(cols = !label_grouping, 
               names_to = "group",
               names_pattern = "(.*)_estimate",
               values_to = "percent") %>% 
  mutate(percent = ifelse(group == "male", percent * -1, percent)) %>% 
  filter(group != "total")

commute_travel$label_grouping <- factor(commute_travel$label_grouping, 
                                        levels = c("Less than 10 minutes", "10 to 14 minutes", "15 to 19 minutes",     
                                                   "20 to 24 minutes","25 to 29 minutes", "30 to 34 minutes",    
                                                   "35 to 44 minutes", "45 to 59 minutes", "60 or more minutes"))


ggplot(commute_travel, aes(x = label_grouping, y = percent, fill = group, color = group)) +
  geom_col(width = 1) +
  scale_x_discrete(limits = rev(levels(commute_travel$label_grouping))) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  coord_flip() +
  annotate(geom = "richtext", 
           label = "<span style = 'color:#EE352E;'><strong>Male</strong></span> and 
           <span style = 'color:#0039A6;'><strong>female</strong></span> commuters have 
           similar commute <br>times, with an average of 41.4 minutes.",
           x = 8.5, y = 17,
           fill = NA,
           label.color = NA,
           size = 3.5,
           family = "Helvetica") +
  annotate(geom = "richtext", 
           label = "<span style = 'color:#FAFEFF;'><strong>More than a quarter of respondents 
           report a commute of an hour or longer.</strong></span>",
           x = 1, y = 0,
           fill = NA,
           label.color = NA,
           size = 3.5,
           family = "Helvetica") +
  labs(
    title = "How long do New Yorkers commute?",
    subtitle = " <br> Data from the American Community Survey & US Census show that workers in New York City commuted on average 41 minutes 
    <br>per day, though a large portion travel at least an hour. Less than 10% of respondents commuted 15 minutes or less.",
    caption = "Created by @kllycttn | Data from the American Community Survey"
  ) +
  theme_plot()

ggsave(here::here("code","2022", "Day9", "day9.png"), width = 11, height = 7, unit = "in", bg = "white")

