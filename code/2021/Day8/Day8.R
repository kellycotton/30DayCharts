# Day 8: Animal
# 04/08/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: https://figshare.com/articles/dataset/AZA_MLE_Jul2018_csv/7539968
# See also:
# Che-Castaldo, J. P., Byrne, A., Perišin, K., & Faust, L. J. (2019). Sex-specific median life 
# expectancies from ex situ populations for 330 animal species. Scientific data, 6(1), 1-7.
# https://www.nature.com/articles/sdata201919


# Setup-----
library(tidyverse)
library(ggbeeswarm)
library(gghighlight)

theme_set(theme_minimal(base_family = "Libre Franklin"))

# Read data
lifespan <- read_csv(here::here("code", "Day8", "animal_lifespan.csv"))

lifespan %>% 
  select(c("Scientific Name", "Male MLE", "Female MLE")) %>% 
  rename(name = `Scientific Name`,
         Male = `Male MLE`,
         Female = `Female MLE`) %>% 
  pivot_longer(cols = -name,
               names_to = "sex", 
               values_to = "life_expectancy") %>% 
  filter(!is.na(life_expectancy)) %>% 
  ggplot(aes(x = sex, y = life_expectancy, color = name)) +
  geom_beeswarm() +
  gghighlight(max(life_expectancy) > 40,  
              label_key = name,
              label_params = list(family = "Bebas Neue")) +
  scale_color_manual(values = c("#7AB1C5", "#99CA7D", "#8CC4B7")) +
  ylab("Median life expectancy, in years") +
  labs(title = "Animal longevity: which species live the longest?",
       subtitle = "Sex-specific median life expectancies for 330 animal species in North American zoos and aquariums. 
<br>Three species have median life expectancies over 40 years: <span style = 'color:#8CC4B7;'>**<em>vultur gryphus</em> (Andean condor)**</span>, 
<br><span style = 'color:#7AB1C5;'>**<em>elephas maximus</em> (Asian elephant)**</span>, and <span style = 'color:#99CA7D;'>**<em>pan paniscus</em> (Bonobo)**</span>.",
       caption = "Created by @kllycttn | Data from Judy Che-Castaldo, Amy Byrne, Kaitlyn Perišin, and Lisa Faust") +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(face = "bold"),
        plot.title = element_text(family = "Bebas Neue", size = 30),
        plot.subtitle = ggtext::element_markdown(lineheight = .2),
        plot.caption = element_text(size = 7)) 
  
ggsave(here::here("code", "Day8", "day8.png"))


