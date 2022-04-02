# Day 1: Part-to-Whole comparison
# 04/01/2021
# https://github.com/Z3tt/30DayChartChallenge_2021


# Setup-----
library(tidyverse)

# Colors & theme
pal <- calecopal::cal_palette("calochortus")
theme_set(theme_minimal(base_family = "Libre Franklin"))

# Data
sports <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

totals <- sports %>% 
  group_by(year) %>% 
  summarise(men_exp = sum(exp_men, na.rm = TRUE),
            women_exp = sum(exp_women, na.rm = TRUE),
            men_rev = sum(rev_men, na.rm = TRUE), 
            women_rev = sum(rev_women, na.rm = TRUE)) %>% 
  pivot_longer(!year, 
               names_to = c("category", "type"), 
               names_pattern = "(.*)_(.*)",
               values_to = "amount")

# number of schools & sports
length(unique(sports$unitid))


ggplot(totals, aes(x = type, y = amount, fill = category)) +
  geom_col() +
  scale_fill_manual(values = pal[4:5], name = NULL, labels = c("Men", "Women")) +
  scale_x_discrete(labels = c("Expenditures", "Revenues")) +
  scale_y_continuous(breaks = scales::breaks_extended(8),
                     labels = scales::label_dollar()) +
  ylab("Amount in USD") +
  facet_wrap(~year, nrow = 1) +
  labs(title = "Collegiate Sports Spending, 2015 - 2019",
       subtitle = "Data collected from 2,141 colleges and universities in the US across 38 differents sports.",
       caption = "Created by @kllycttn | Data from Equity in Athletics Data Analysis") +
  theme(
    plot.title = element_text(family = "Arvo", size = 29),
    axis.title.x = element_blank(), 
    legend.position = "bottom", 
    panel.grid.major.x = element_blank()
  )

ggsave(here::here("code","2022", "Day1", "day1.png"), width = 11, height = 8, unit = "in")


