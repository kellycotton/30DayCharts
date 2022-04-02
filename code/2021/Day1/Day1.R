# Day 1: Part-to-Whole comparison
# 04/01/2021
# https://github.com/Z3tt/30DayChartChallenge_2021


# Setup-----
library(tidyverse)
library(ggrepel)
library(calecopal)
library(colorspace)
library(ggdark)

# Function for label positions
cs_fun <- function(x){(cumsum(x) + c(0, cumsum(head(x , -1))))/ 2}

# Colors
pal <- cal_palette("superbloom3")

# Read data
netflix <- read_csv(here::here("code", "Day1", "netflix_titles.csv"))

# Add continents
asia <-  c('Afghanistan', 'Azerbaijan', 'Bangladesh', 'Cambodia', 'China', 'Hong Kong', 'India', 
           'Indonesia', 'Iran', 'Iraq', 'Israel', 'Japan', 'Jordan', 'Kazakhstan', 'Kuwait', 
           'Lebanon', 'Malaysia', 'Mongolia', 'Nepal', 'Pakistan', 'Philippines', 'Qatar', 
           'Saudi Arabia', 'Singapore', 'South Korea', 'Sri Lanka', 'Syria', 'Taiwan', 'Thailand', 
           'United Arab Emirates', 'Vietnam')
south_america <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Paraguay', 'Peru', 
                   'Uruguay', 'Venezuela')
oceania <- c('Australia', 'New Zealand', 'Samoa')
europe <- c('Albania', 'Armenia', 'Austria', 'Belarus', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 
            'Czech Republic', 'Denmark', 'East Germany', 'Finland', 'France', 'Georgia', 'Germany',
            'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Latvia', 'Liechtenstein', 
            'Lithuania', 'Luxembourg', 'Malta', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 
            'Portugal', 'Romania', 'Russia', 'Serbia', 'Slovakia', 'Slovenia', 'Soviet Union', 
            'Spain', 'Sweden', 'Switzerland', 'Turkey', 'Ukraine', 'United Kingdom', 
            'Vatican City', 'West Germany')
africa <- c('Algeria', 'Angola', 'Botswana', 'Egypt', 'Ghana', 'Kenya', 'Malawi', 'Mauritius',
            'Morocco', 'Namibia', 'Nigeria', 'Senegal', 'Somalia', 'South Africa', 'Sudan', 
            'Uganda', 'Zimbabwe')
north_america <- c('Bahamas', 'Bermuda', 'Canada', 'Cayman Islands', 'Cuba', 'Dominican Republic', 
                   'Guatemala', 'Jamaica', 'Mexico', 'Nicaragua', 'Panama', 'Puerto Rico', 'United States')

# Separate entries with multiple countries and add continent info
country_data <- netflix %>% 
  filter(!is.na(country)) %>%  # filter any without a country
  select(c("type", "country", "show_id")) %>% 
  mutate(country = strsplit(country, ", ")) %>% 
  unnest(country) %>% 
  mutate(country = gsub("[[:punct:]]","" , country)) %>% 
  mutate(continent = case_when(
    country %in% africa ~ "Africa", 
    country %in% asia ~ "Asia", 
    country %in% europe ~ "Europe", 
    country %in% north_america ~ "North America", 
    country %in% oceania ~ "Oceania",
    country %in% south_america ~ "South America"
  ))

# Format data for inner circle, add color info
inner_data <- country_data %>% 
  group_by(continent) %>% 
  summarise(total = n()) %>% 
  ungroup() %>% 
  mutate(ymax = cumsum(total), 
         ymin = lag(ymax, n = 1, default = 0)) %>% 
  mutate(color = pal,
         colorL = coords(as(hex2RGB(color), "polarLUV"))[,1],
         colorC = coords(as(hex2RGB(color), "polarLUV"))[,2],
         colorH = coords(as(hex2RGB(color), "polarLUV"))[,3])

# Plot for inner circle
inner <- ggplot(inner_data) +
  geom_rect(aes(xmin = 2, xmax = 3, ymin = ymin, ymax = ymax),
            color = "white",
            fill = inner_data$color,
            size = .07) +
  geom_text(aes(label = str_wrap(paste0(continent), 1),
                    x = ifelse(continent == "South America", 2.75, 2.5),
                    y = cs_fun(total)),
            size = 3,
            color = "white",
            lineheight = .65,
            family = "Bebas Neue") +
  xlim(0, 6)

# Format data for out circle, add color info
outer_data <- country_data %>% 
  group_by(continent, country) %>% 
  summarise(total_country = n()) %>% 
  left_join(inner_data %>% select(continent, total, colorL, colorC, colorH)) %>% 
  ungroup() %>% 
  mutate(ymax = cumsum(total_country), 
         ymin = lag(ymax, n = 1, default = 0)) %>% 
  group_by(continent) %>% 
  mutate(max = ifelse(total_country == max(total_country), TRUE, FALSE),
         outercolor = hex(polarLUV(colorL * 1.2, colorC * 0.5, colorH)))

# Plot for outer circle
outer <- geom_rect(data = outer_data, 
                   aes(xmin = 3, 
                       xmax = 4, 
                       ymin = ymin, 
                       ymax = ymax),
            fill = outer_data$outercolor,
            color = "white",
            size = .07)

# Plot inner + outer circle together + make circle
inner +  outer +
  geom_label_repel(data = filter(outer_data, max == TRUE), 
                   aes(label = str_wrap(country, 2), x = 4, y = (ymax + ymin)/2),
                   max.overlaps = Inf, 
                   box.padding = 0.5,
                   min.segment.length = 0,
                   nudge_x = 1,
                   nudge_y = 1,
                   segment.curvature = -1e-20,
                   arrow = arrow(length = unit(0.01, "npc")),
                   show.legend = FALSE,
                   family = "Bebas Neue"
  ) +
  coord_polar(theta = "y") +
  labs(title = "Netflix across the world",
       subtitle = "As of 2019, Netflix had over 7,000 movies and TV shows on its platform.
Where are these titles produced?",
       caption = "Created by @kllycttn | Data from Flixable") +
  dark_theme_void() +
  theme(
    plot.title = element_text(family = "GraphiqueW01-Regular", size = 32, hjust = 0.5),
    plot.subtitle = element_text(family = "Bebas Neue", size = 14, hjust = 0.5),
    plot.caption = element_text(family = "Bebas Neue", size = 9)
  ) 


ggsave(here::here("code", "Day1", "day1.png"), width = 11, height = 8, unit = "in")

       