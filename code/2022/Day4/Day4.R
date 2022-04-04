# Day 4: Flora
# 04/04/2021
# https://github.com/30DayChartChallenge/Edition2022


# Setup-----
library(tidyverse)
library(treemap)
library(patchwork)
library(ggfittext)

<<<<<<< HEAD
theme_set(theme_minimal(base_family = "Lora"))
=======
theme_set(theme_minimal(base_family = "Noto Serif"))
>>>>>>> a9a6b4bcc51314261e89b90a8c698c7e0afbcf46


# Read data
plants <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')

plant_agg <- plants %>% 
  group_by(continent) %>% 
  summarize(count = n())

# Make treemap of all continents
data_tree_cont <- treemap(plant_agg,
                     index="continent",
                     vSize="count",
                     type="index",
                     algorithm = "pivotSize",
                     border.lwds = 2,
                     border.col = "white",
                     aspRatio = 6/3,
                     palette = c("#244229", "#12263A", "#E9D758", "#FF8552", "#9097C0", "#DB2B39"))

# make it more ggplot friendly
# shoutout to June Choe 
# https://yjunechoe.github.io/posts/2020-06-30-treemap-with-ggplot/
cont_map_data <- data_tree_cont$tm %>% 
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>% 
  # get center coordinates for labels and add label info
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2,
         label_name = str_glue("{continent}\n({vSize})")) %>% 
  # mark primary groupings and set boundary thickness
  mutate(primary_group = ifelse(is.na(continent), 1.2, .5)) %>% 
  # remove colors from primary groupings (since secondary is already colored)
  mutate(color = ifelse(is.na(continent), NA, color)) 

p1 <- ggplot(cont_map_data, aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  # add fill and borders for groups and subgroups
  statebins:::geom_rrect(aes(fill = color, size = primary_group),
            show.legend = FALSE, color = "white", alpha = .9) +
  scale_fill_identity() +
  # set thicker lines for group borders
  scale_size(range = range(cont_map_data$primary_group)) +
  # add labels
<<<<<<< HEAD
  ggfittext::geom_fit_text(aes(label = label_name), 
                           min.size = 1, 
                           reflow = TRUE,
                           family = "Jost",) +
=======
  ggfittext::geom_fit_text(aes(label = label_name), min.size = 1, reflow = TRUE) +
>>>>>>> a9a6b4bcc51314261e89b90a8c698c7e0afbcf46
  # options
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void()

# Treemap just Africa
plant_africa <- plants %>% 
  filter(continent == "Africa") %>% 
  group_by(country) %>% 
  summarize(count = n())

data_tree_africa <- treemap(plant_africa,
                          index="country",
                          vSize="count",
                          type="index",
                          algorithm = "pivotSize",
                          border.lwds = 2,
                          aspRatio = 6/3,
                          palette = "#244229",
                          radius = grid::unit(90, "pt"))

africa_map_data <- data_tree_africa$tm %>% 
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>% 
  # get center coordinates for labels
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2,
         label_name = str_glue("{country}\n({vSize})")) %>% 
  # mark primary groupings and set boundary thickness
  mutate(primary_group = ifelse(is.na(country), 1.2, .5)) %>% 
  # remove colors from primary groupings (since secondary is already colored)
  mutate(color = ifelse(is.na(country), NA, color))

p2 <- ggplot(africa_map_data, aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  # add fill and borders for groups and subgroups
  statebins:::geom_rrect(aes(fill = color, size = primary_group),
            show.legend = FALSE, color = "white", alpha = .7) +
  scale_fill_identity() +
  # set thicker lines for group borders
  scale_size(range = range(africa_map_data$primary_group)) +
  # add labels
<<<<<<< HEAD
  ggfittext::geom_fit_text(aes(label = label_name), 
                           min.size = 1, 
                           reflow = TRUE,
                           family = "Jost",) +
=======
  ggfittext::geom_fit_text(aes(label = label_name), min.size = 1, reflow = TRUE) +
>>>>>>> a9a6b4bcc51314261e89b90a8c698c7e0afbcf46
  # options
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void()

# Draw polygon
d=data.frame(x=c(.98,1,1.4,2), y=c(1,1.5,1.5,1))
p3 <- ggplot() +
  geom_polygon(data=d, mapping=aes(x=x, y=y), fill = "#244229", alpha = 0.3) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    panel.grid = element_blank(),
    legend.position = "none"
  )


p1/ p3 /p2  +
  plot_annotation(
<<<<<<< HEAD
    title = " Plants in Danger: Where are plants going extinct?",
    subtitle = " Across the globe, plants are experiencing extensive biodiversity loss. However, the impact of this loss is not equally spread 
across the continents. As of 2020, 500 plants species are considered extinct, with nearly 20% from Madagascar alone.",
    caption = " Created by @kllycttn | Data from the International Union for Conservation of Nature "
  ) +
  plot_layout(heights = c(2,1,2)) &
  theme(
    plot.title = element_text(size = 25, hjust = 0.5),
    plot.subtitle = element_text(family = "Jost", hjust = 0.5, size = 12.5),
    plot.margin = margin(0.01, 0, 0, 0, unit = "in")
  ) 

ggsave(here::here("code","2022", "Day4", "day4.png"), width = 11, height = 8, unit = "in", bg = "white")

=======
    title = "Where are the extinct plants?",
    subtitle = "",
    caption = "Created by @kllycttn | Data from the International Union for Conservation of Nature"
  ) +
  plot_layout(heights = c(2,1,2)) &
  theme(
    plot.title = element_text(size = 25),
    plot.margin = margin(0, 0, 0, 0, unit = "in")
  ) 

>>>>>>> a9a6b4bcc51314261e89b90a8c698c7e0afbcf46
