# Day 5: Slope
# 04/05/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: https://nsaa.org/webdocs/Media_Public/IndustryStats/Historical_Skier_Days_1979_1920.pdf


# Setup-----
library(pdftools)
library(stringr)
library(tidyverse)
library(hrbrthemes)

# Color palette 
pal <- nycpalettes::nyc_palette("Sunset1")

# Read data from pdf
skiing <- pdf_text("https://nsaa.org/webdocs/Media_Public/IndustryStats/Historical_Skier_Days_1979_1920.pdf")

# Data cleaning----
# Split text into rows, trim white space, make into data frame with specific column names
skiing <- trimws(strsplit(skiing, "\n")[[1]]) 
skiing <- str_split_fixed(skiing[7:48], " {2,}", 10)
skiing <- data.frame(skiing, stringsAsFactors = FALSE) 
names(skiing) <- c("season",
                     "northeast",
                     "southeast",
                     "midwest",
                     "rocky_mtn",
                     "pacific_southwest",
                     "pacific_northwest",
                     "pacific_west_total",
                     "national_total",
                     "national_rank")

# Format data for plotting
ski_data <- skiing %>% 
  select(-c("pacific_west_total", "national_total", "national_rank")) %>% 
  na_if("Not avail.") %>% 
  drop_na() %>% 
  pivot_longer(cols = -season,
               names_to = "region",
               values_to = "visitors",
               values_transform = list(visitors = as.integer))

# Plotting----
ggplot(ski_data, aes(x = season, y = visitors, color = region, group = region)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = pal) +
  coord_cartesian(xlim = c(0, 26.5)) +
  theme_ft_rc() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45)) +
  labs(title = "A comparison of slopes: Snowsports visitors across the US",
       subtitle = "Number of visitors by region (in millions), 1995 - 2020.",
       caption = "Created by @kllycttn | Data from National Ski Areas Association") +
  xlab("Season") + ylab("Visitors (in millions)") +
  annotate(geom = "text",
           x = 25.5,
           y = 22,
           label = str_wrap("Rocky Mountains", 1),
           color = "#787B8E",
           lineheight = .7, 
           fontface = "bold") +
  annotate(geom = "text",
           x = 25.5,
           y = 11.8,
           label = "Northeast",
           color = "#8293A3", 
           fontface = "bold") +
  annotate(geom = "text",
           x = 25.5,
           y = 7.5,
           label = str_wrap("Pacific Southwest", 1),
           color = "#9C9CA8",
           lineheight = .7, 
           fontface = "bold") +
  annotate(geom = "text",
           x = 25.5,
           y = 6,
           label = "Midwest",
           color = "#E6DDC0", 
           fontface = "bold") +
  annotate(geom = "text",
           x = 25.5,
           y = 4.5,
           label = "Southeast",
           color = "#7A726F", 
           fontface = "bold") +
  annotate(geom = "text",
           x = 25.5,
           y = 3,
           label = str_wrap("Pacific Northwest", 1),
           color = "#748CA4",
           lineheight = .7, 
           fontface = "bold")

ggsave(here::here("code", "Day5", "day5.png"), width = 10, height = 7, units = "in")
