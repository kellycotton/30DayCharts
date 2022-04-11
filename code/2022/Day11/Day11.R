# Day 11: Circular
# 04/11/2022
# https://github.com/Z3tt/30DayChartChallenge_2022
# Data: MTA Bridges and Tunnels, data.NY.gov
# https://data.ny.gov/Transportation/Hourly-Traffic-on-Metropolitan-Transportation-Auth/qzve-kjga

# Setup----
library(tidyverse)
library(ggtext)
library(geomtextpath)

# Build theme----
theme_plot <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#949396", color = "#949396"),
          panel.background = element_rect(fill = "#949396", color = "#949396"),
          text = element_text(color = "#36343B", family = "Helvetica"), 
          plot.title = element_text(color = "#36343B", hjust = 0.5, size = 22, family = "Helvetica", face = "bold"),
          plot.subtitle = element_markdown(hjust = 0.5, color = "#36343B", size = 13, family = "Helvetica", vjust = -0.5),
          plot.caption = element_text(color = "#36343B", size = 8, hjust = 1, family = "Helvetica"),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "#36343B", size = 13.5, face = "bold"),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_line(color = "#DDDCE2"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "top")
}

# Data
mta <- read_csv(here::here("code", "2022", "Day11", "hourly_mta.csv"))
mta <- janitor::clean_names(mta) 

mta_summary <- mta %>% 
  rowwise() %>% 
  mutate(total = number_vehicles_e_z_pass + number_vehicles_v_toll) %>% 
  group_by(hour, direction) %>%
  summarize(total_avg = mean(total))

mta_summary %>% 
  ungroup() %>% 
  summarize(sum(total_avg))

axis_text <- tibble(
  x = c(rep(14, 6)),
  y = c(seq(0, 2500, 500))
)

# Plot 
ggplot(mta_summary, aes(x = hour, y = total_avg, fill = direction, color = direction)) +
  geom_density(stat = "identity", alpha = 0.7) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20),
                     labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8PM")) +
  scale_y_continuous(breaks = c(seq(0, 3000, 500)),
                     limits = c(0, 3000)) +
  scale_fill_manual(values = c("#748DBD", "#443850"), name = "Direction", labels = c("Inbound", "Outbound")) +
  scale_color_manual(values = c("#443850", "#748DBD"), name = "Direction", labels = c("Inbound", "Outbound")) +
  ylab("Number of Vehicles") +
  labs(
    title = "How many vehicles cross NYC bridge and tunnels each day?",
    subtitle = "<br>Over the course of an average day, 79,972 vehicles cross the New York City bridge and tunnels.",
    caption = "Created by @kllycttn | Data from the MTA Bridges and Tunnels via NYC Open Data"
  ) +
  coord_curvedpolar() +
  geom_text(data = axis_text, aes(x = x, y = y, label = y), inherit.aes = FALSE,
            size = 3,
            fontface = "bold") +
  annotate(geom = "text", label = "Number of Vehicles", x = 14.7, y = 1500, angle = 45, fontface = "bold") +
  theme_plot()

ggsave(here::here("code","2022", "Day11", "day11.png"), width = 10, height = 8, unit = "in", bg = "#949396")

