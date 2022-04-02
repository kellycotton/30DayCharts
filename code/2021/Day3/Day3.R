# Day 3: Historical
# 04/03/2021
# https://github.com/Z3tt/30DayChartChallenge_2021
# Data: https://data.cdc.gov/NCHS/Monthly-Counts-of-Deaths-by-Select-Causes-2020-202/9dzk-mvmi

# Setup-----
library(tidyverse)

# Read data
deaths <- read_csv(here::here("code", "Day3", "Deaths_2020.csv")) %>% 
  select(-starts_with("flag")) 

colnames(deaths) <- c("data_date", "start_date", "end_date", "state", "year", "month", 
                      "all_cause", "natural_cause", "septicemia", "malignant_neoplasms", 
                      "diabetes", "alz_disease", "influenza_pneumonia", "chronic_lower_resp",
                      "other_resp", "nephritis","other", "heart_disease", "cv_disease", "accidents", 
                      "vehicle_accidents", "suicide", "homicide", "overdose", "covid_other", "covid_only")

dat <- deaths %>% 
  filter(state == "United States" & year == "2020") %>% 
  select(state, year, month, all_cause, heart_disease, starts_with("covid")) %>% 
  rowwise() %>% 
  mutate(covid = covid_other + covid_only,
         all_other = all_cause - covid - heart_disease) %>% 
  select(-c(all_cause, covid_other, covid_only)) %>% 
  pivot_longer(cols = c("heart_disease", "covid", "all_other"),
               names_to = "cause",
               values_to = "number") %>% 
  group_by(month) %>% 
  mutate(label_y = sum(number) + 40000,
         month = factor(month.abb[month], levels = month.abb))

ggplot(dat, aes(x = month, y = number, fill = cause)) +
  geom_col(color = "#5B5A5A",
           width = 1) +
  geom_text(aes(label = month, y = label_y), 
            family = "Deckhouse Regular") +
  scale_fill_manual(name = NULL,
                    values = c("#87c0e6", "#ffa0aa", "#808080"),
                    labels = c("All other deaths", "COVID-19", "Heart disease")) +
  labs(title = "Diagram of the Causes of Mortality",
       subtitle = "IN THE UNITED STATES IN 2020. \nDesign inspired by Florence Nightingale's \"rose\" diagram.",
       caption = "Created by @kllycttn | Data from CDC") +
  theme_void() + 
  theme(
    legend.position = c(.5,.1),
    legend.direction = "horizontal",
    legend.text = element_text(family = "Deckhouse Regular"),
    plot.title = element_text(family = "MFC Noir Monogram Solid", hjust = 0.5, size = 25),
    plot.subtitle = element_text(family = "Deckhouse Regular", hjust = 0.5, size = 16)
  ) +
  coord_polar()

ggsave(here::here("code", "Day3", "day3.png"))

