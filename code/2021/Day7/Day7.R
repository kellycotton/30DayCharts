# Day 7: Physical
# 04/07/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: Personal

# Setup-----
library(tidyverse)
library(lubridate)
library(ggridges)

set.seed(1)
theme_set(theme_minimal(base_family = "Lora"))

data <- read_csv(here::here("code", "Day7", "Health Data.csv"))
dates <- dmy_hm(data$Start)
dates <-  data.frame(date = format(dates, format = "%m-%d"), Year= format(dates, format = "%Y"))

activity_data <- cbind(data, dates)
colnames(activity_data) <-c("start", "finish", "calories", "distance", "steps", "date", "year")

# Find the top dates
activity_data %>% 
  slice_max(order_by = steps, n = 5)

# Find total
activity_data %>% 
  summarise(sum(steps))

# Set arrow coordinates
arrows <- 
  tibble(
    x1 = c(25000, 25000, 25000, 28000, -1000, 28000), 
    x2 = c(25538, 22500, 20550, 27100, 700, 25000),
    y1 = c(1.38, 1.38, 1.38, 3.25, 3.3, 3.1),
    y2 = c(.67, .68, .69, 3.5, 3.85, 2.8)
  )
# Color palette
pal <- c("#82AEB1", "#F1D302", "#E94F37", "#656176", "#EE964B")

activity_data %>% 
  ggplot(aes(x = steps, y = year, color = year, fill = year)) +
  geom_density_ridges(jittered_points = TRUE, 
                      position = position_points_jitter(width = 0.02, yoffset = -.25, seed = 1),
                      alpha = 0.6) +
  annotate(geom = "text",
           label = "Ran a long race", 
           x = 28000,
           y = 3.2, 
           family = "Lora") +
  annotate(geom = "text",
           label = "Friends visiting NYC", 
           x = 25000,
           y = 1.5, 
           family = "Lora") +
  annotate(geom = "text",
           label = "A global pandemic", 
           x = -1000,
           y = 3.2, 
           family = "Lora") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3, 
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(labels = scales::label_comma(suffix = " steps")) +
  labs(title = "How many steps have I taken since 2017?",
       subtitle = "Since July 2017, I have tracked the number of steps I've taken (almost) every day. In a little over 4 years, I have 
\ntaken **9,232,798** steps. This includes days spent walking around New York with visiting friends, running a 
\nhalf-marathon, and a pandemic that dropped my step count to nearly 0.",
       caption = "Created by @kllycttn | Data from personal health data") +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.title = element_blank(), 
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = ggtext::element_markdown()
  )

  
ggsave(here::here("code", "Day7", "day7.png"), width = 9, height = 6.5, units = "in")

