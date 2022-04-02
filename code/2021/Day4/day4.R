# Day 4: Magical
# 04/04/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: https://osf.io/52x8u/
# See Also: Olson J. A., Amlani A. A., Rensink R. A. (2012). Perceptual and cognitive 
# characteristics of common playing cards. Perception, 41(3), 268–286. doi:10.1068/p7175

# Setup-----
library(tidyverse)

# Read data
card <- read_csv(here::here("code", "Day4", "pickacard.csv"))

# Make cards factors for best ordering in plot 
card$card1 <- factor(card$card1, levels = c(
  "AS", "AH", "AC", "AD",
  "KS", "KH", "KC", "KD",
  "QS", "QH", "QC", "QD",
  "JS", "JH", "JC", "JD",
  "10S", "10H", "10C", "10D",
  "9S", "9H", "9C", "9D",
  "8S", "8H", "8C", "8D",
  "7S", "7H", "7C", "7D",
  "6S", "6H", "6C", "6D",
  "5S", "5H", "5C", "5D",
  "4S", "4H", "4C", "4D",
  "3S", "3H", "3C", "3D",
  "2S", "2H", "2C", "2D"
))

# Find most common card
card %>% 
  filter(!is.na(card1)) %>% 
  group_by(card1) %>% 
  summarize(count = n()) %>% 
  mutate(perc = (count/sum(count)) * 100) %>% 
  slice_max(order_by = perc)

# Find least common card
card %>% 
  filter(!is.na(card1)) %>% 
  group_by(card1) %>% 
  summarize(count = n()) %>% 
  mutate(perc = (count/sum(count)) * 100) %>% 
  slice_min(order_by = perc)

# Organize data for plotting
card_pick <- card %>% 
  filter(!is.na(card1)) %>% 
  group_by(gender, card1) %>% 
  summarize(count = n()) %>% 
  group_by(gender) %>% 
  mutate(prop = count/sum(count)) %>% 
  mutate(count = ifelse(gender == "m", count * -1, count),
         perc = ifelse(gender == "m", prop * -100, prop*100),
         nudge = ifelse(gender == "m", -.8, .8), # To offset the text from the end of the bar
         card_color = ifelse(str_detect(card1, "H|D"), "#B8000A", "#000000"))

# Subtitle
sub = "Researchers asked 1,354 people to name an arbitrary playing card. <br>
Most people (20%) picked the <strong>Ace of Spades</strong>. 
Male participants (15%) <br> were more than twice as likely to pick the 
<span style = 'color:#B8000A;'><strong>Queen of Hearts</strong></span> compared to 
<br>female participants (6%). 
The least likely cards were each chosen by only <br>two people: the <strong>9 of Spades</strong> 
and the <strong>9 of Clubs</strong>."

# Create plot
ggplot(card_pick, aes(x = card1, y = perc, fill = gender)) +
  geom_col(width = .9) + 
  geom_text(aes(label = card1, color = card_color), nudge_y = card_pick$nudge, 
            family = "Libre Franklin",
            size = 3, 
            fontface = "bold") + 
  scale_x_discrete(limits = rev(levels(card_pick$card1))) +
  scale_y_continuous(breaks = seq(-20, 20, by = 5),
    labels = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20), "%")) +
  scale_color_identity() +
  scale_fill_manual(name = NULL,
                    breaks = c("m", "f"),
                    labels = c("Male", "Female"),
                    values = c("#A09D9D", colorspace::lighten("#A09D9D", amount = 0.4))) +
  coord_flip() +
  labs(title = "Pick a card, any card", 
       subtitle = sub,
       caption = "Created by @kllycttn | Data from Jay Olson, Amy Amlani, & Ronald Rensink") +
  annotate(geom = "text",
           x = 17,
           y = 8.3,
           label = "A Ace",
           family = "Libre Franklin",
           fontface = "bold") +
  annotate(geom = "text",
           x = 15,
           y = 8.4,
           label = "K King",
           family = "Libre Franklin",
           fontface = "bold") +
  annotate(geom = "text",
           x = 13,
           y = 8.7,
           label = "Q Queen",
           family = "Libre Franklin",
           fontface = "bold") +
  annotate(geom = "text",
           x = 11,
           y = 8.4,
           label = "J Jack",
           family = "Libre Franklin",
           fontface = "bold") +
  annotate(geom = "text",
           x = 17,
           y = 11.7,
           label = "S Spades",
           family = "Libre Franklin",
           fontface = "bold") +
  annotate(geom = "text",
           x = 15,
           y = 11.6,
           label = "H Hearts",
           family = "Libre Franklin",
           fontface = "bold",
           color = "#B8000A") +
  annotate(geom = "text",
           x = 13,
           y = 11.45,
           label = "C Clubs",
           family = "Libre Franklin",
           fontface = "bold") +
  annotate(geom = "text",
           x = 11,
           y = 12.1,
           label = "D Diamonds",
           family = "Libre Franklin",
           fontface = "bold",
           color = "#B8000A") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(family = "Card Characters Narrow Figures Regular", 
                              size = 26),
    plot.subtitle = ggtext::element_markdown(family = "Libre Franklin"),
    legend.position = c(.71, .4),
    legend.direction = "horizontal"
  )

ggsave(here::here("code", "Day4", "day4.png"), width = 12, height = 7.5, units = "in")

