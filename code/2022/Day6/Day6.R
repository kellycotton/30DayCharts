# Day 6: Our World in Data
# 04/05/2021
# https://github.com/30DayChartChallenge/Edition2022
# Data from Our World in Data


# Setup-----
library(tidyverse)
library(countrycode)
library(ggtext)
library(patchwork)

theme_set(theme_minimal(base_family = "Assistant"))

# Read data
water <- read_csv(here::here("code", "2022", "Day6", "water-and-sanitation.csv"))

water_clean <- janitor::clean_names(water) %>% 
  select(entity, year, contains ("water")) %>% 
  filter(year == 2020) %>% 
  filter(!is.na(access_to_safely_managed_drinking_water)) %>% 
  mutate(continent = countrycode(sourcevar = entity, 
                              origin = "country.name",
                              destination = "continent")) %>% 
  filter(!is.na(continent)) 

world_avg <- water_clean %>% 
  summarise(world_mean = mean(access_to_safely_managed_drinking_water))

continent_avg <- water_clean %>% 
  group_by(continent) %>% 
  summarise(continent_mean = mean(access_to_safely_managed_drinking_water))

bottom <- water_clean %>% 
  group_by(continent) %>% 
  slice_min(order_by = access_to_safely_managed_drinking_water, n = 5) %>% 
  left_join(., continent_avg) %>% 
  select(c(entity, access_to_safely_managed_drinking_water, continent_mean, continent)) 

water_plot <- ggplot(bottom, aes(x = reorder(entity, access_to_safely_managed_drinking_water), y = access_to_safely_managed_drinking_water, fill = continent)) +
  geom_col(aes(y = continent_mean), alpha = 0.5, fill = "gray") +
  geom_col(width = 0.5) +
  geom_hline(yintercept = world_avg$world_mean) + 
  facet_wrap(~continent, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("#535f77", "#358375", "#f6d564", "#fea33a", "#c34f62")) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), 
                     expand = c(0.02, 0), limits = c(0, 100)) +
  ylab("Share of country population with access to a safely managed drinking water source") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 18, face = "bold", family = "Merriweather")
  )

# legend plot
# shout-out to https://themockup.blog/posts/2020-11-29-bullet-chart-variants-in-r/

label_df <- tibble(
  x = c(15, 15, 87),
  y = c(1.6, 0.35, .55),
  label = c("Continent Average ", "Country Access ", "World Average")
)

seg_df <- tibble(
  x1 = c(0.2, 90, 0.2, 74.8, 94),
  x2 = c(0.2, 90, 0.2, 74.8, 94),
  y1 = c(1.3, 1.3, rep(.7, 2), .75),
  y2 = c(1.61, 1.61, rep(.343, 2), 1.25)
  
)

seg2_df <- tibble(
  x1 = c(0.2, 0.2),
  x2 = c(90, 74.8),
  y1 = c(1.6, .35),
  y2 = c(1.6, .35)
)

legend_plot <- tibble(
  x = 75,
  y = factor("Y"),
  x2 = 90
) %>%
  ggplot(aes(x = x, y = y)) +
  geom_col(aes(x = 100), fill = "white", color = "grey", width = 0.4) +
  geom_col(aes(x = x2), width = 0.5, fill = "gray", alpha = 0.5) +
  geom_col(width = 0.2, color = "#535f77", fill = "#535f77") +
  geom_segment(
    data = seg_df,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    color = c(rep("black", 5)),
    size = 1
  ) +
  geom_segment(
    data = seg2_df,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    color = c("black", "black"),
    size = 1
  ) +
  geom_label(
    data = label_df,
    aes(x = x, y = y, label = label),
    hjust = 0, size = 5, fontface = "bold", fill = "white",
    color = "black",
    label.size = NA,
    #family = "Oswald",
    label.padding = unit(0.05, "lines"),
  ) +
  theme_void() +
  theme(
    plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"),
    plot.caption = element_markdown(size = 11)
  ) +
  coord_cartesian(ylim = c(0.7, 1.2), xlim = c(0, 108)) +
  labs(
    caption = "Created by @kllycttn | Data from WHO/UNICEF JMP for Water Supply and Sanitation via Our World in Data"
  )

water_plot / legend_plot & plot_layout(heights = c(14,1)) &
  plot_annotation(
    title = "How much of the world's population has access to safe drinking water in 2020?",
    subtitle = "Safely managed drinking water is defined as “Improved source located on premises, available when needed, and free from microbiological 
and priority chemical contamination.” The data below show the countries with the lowest share of the population with safe water-access for 
each continent. Each colored bar represents the share of the population for that country and the gray bars indicate the average access for the 
continent. The black line indicates the world average: 75%.",
    theme = theme(
      plot.title = element_text(size = 30, family = "Merriweather"),
      plot.subtitle = element_text(size = 20)
    )
  )

ggsave(here::here("code","2022", "Day6", "day6.png"), height = 16, width = 17, units = "in", bg = "white")


