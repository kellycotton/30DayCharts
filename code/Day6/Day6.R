# Day 6: Experimental
# 04/05/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: 

# Setup-----
library(tidyverse)
library(ggalt)

theme_set(theme_minimal(base_family = "Neutra Text"))

# Read data
forest <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')

# Find the top countries
countries <- forest %>% 
  filter(year %in% c("1990", "2015")) %>% 
  group_by(entity) %>% 
  select(-code) %>% 
  pivot_wider(id_cols = "entity",
              names_from = "year", 
              names_prefix = "year_",
              values_from = "net_forest_conversion") %>% 
  mutate(diff = abs(year_2015 - year_1990)) %>% 
  ungroup() %>% 
  slice_max(n = 5, order_by = diff) %>% 
  pull(entity)
  
dat <- forest %>% 
  filter(entity %in% countries) %>% 
  filter(year %in% c("1990", "2015")) %>% 
  select(-code) %>% 
  pivot_wider(id_cols = "entity",
              names_from = "year", 
              names_prefix = "year_",
              values_from = "net_forest_conversion") %>% 
  arrange(desc(year_1990)) %>% 
  mutate(y = row_number()) %>% 
  rowwise() %>% 
  mutate(xend_1 = ifelse(y %% 2 == 0, (year_1990 - 70000), (year_1990 + 70000)),
         xend_2 = ifelse(y %% 2 == 0, (year_2015 + 70000), (year_2015 - 70000)),
         yend_1 = ifelse(y %% 2 == 0, y - 0.08, y + 0.08), 
         yend_2 = ifelse(y %% 2 == 0, y + 0.08, y - 0.08))

ggplot(dat, aes(x = year_1990, xend = year_2015, y = reorder(entity, y), group = entity)) +
  geom_dumbbell(colour_x ="#3B5249",
                size_x = 0.6,
                size = 0.75, 
                color = "#3B5249",
                colour_xend ="#3B5249",
                size_xend = 0.6) +
  geom_text(label = "1990", nudge_y = .25, hjust = 1, size = 3, 
            fontface = "bold",
            family = "Neutra Text") +
  geom_text(aes(x = year_2015), label = "2015", 
            nudge_y = .25, hjust = 0, size = 3, 
            fontface = "bold",
            family = "Neutra Text") +
  scale_x_continuous(labels = scales::label_comma()) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = scales::alpha("#93C0A4", 0.5), color = "#93C0A4"),
    panel.background = element_rect(fill = scales::alpha("#93C0A4", 0.8), color = "#93C0A4"),
    panel.grid = element_line(color = scales::alpha("#2F1000", 0.1)),
    axis.text = element_text(face = "bold")
  ) +
  labs(title = "A forest of illusions: Comparing deforestation rates between 1990 and 2015",
       subtitle = "Net change in forest cover in the preceding 5 years, across the world and in the five countries that experienced the largest change between 1990 and 2015. 
Net change measures any gains in forest cover (natural forest expansion or tree-planting) - deforestation. More negative net change indicates the country is 
losing more than they are able to restore. The arrows are meant to recreate the Muller-Lyer illusion, an optical illusion that may be influenced by our 
environment. Research has found that people from rural areas are much less susceptible to the illusion compared to people from urban areas.",
       caption = "Created by @kllycttn | Data from Hannah Ritchie and Max Roser, Our World in Data | #TidyTuesday") +
  annotate(geom = "segment",
           x = dat$year_1990,
           xend = dat$xend_1,
           y = dat$y,
           yend = dat$yend_1, 
           color = "#3B5249",
           size = 0.75) +
  annotate(geom = "segment",
           x = dat$year_1990,
           xend = dat$xend_1,
           y = dat$y,
           yend = dat$yend_2, 
           color = "#3B5249",
           size = 0.75) +
  annotate(geom = "segment",
           x = dat$year_2015,
           xend = dat$xend_2,
           y = dat$y,
           yend = dat$yend_1, 
           color = "#3B5249",
           size = 0.75) +
  annotate(geom = "segment",
           x = dat$year_2015,
           xend = dat$xend_2,
           y = dat$y,
           yend = dat$yend_2, 
           color = "#3B5249",
           size = 0.75)

ggsave(here::here("code", "Day6", "day6.png"), width = 9.8, units = "in")

