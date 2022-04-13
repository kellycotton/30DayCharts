# Day 13: 
# 04/13/2022
# https://github.com/Z3tt/30DayChartChallenge_2022
# Data: Spotify
# https://developer.spotify.com/

# Setup-----
library(tidyverse)
library(spotifyr)
library(gridExtra)

# Build theme----
theme_plot <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
          panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
          text = element_text(color = "#191414", family = "Gotham-Light"), 
          plot.title = element_text(color = "#191414", hjust = 0.5, size = 22, family = "Gotham-Bold", face = "bold"),
          plot.subtitle = element_markdown(hjust = 0.5, color = "#191414", size = 13, family = "Gotham-Light", vjust = -0.5),
          plot.caption = element_markdown(color = "#191414", size = 8, hjust = 1, family = "Gotham-Medium"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(color = "#7D7D7D", size = 13.5, face = "bold"),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          axis.line = element_line())
}

# Data

top <- get_category_playlists("toplists")

playlist_username <- 'spotify'

# Top Hits
playlist_uris <- gsub(".*:", "", top[1,]$uri, fixed = FALSE)
playlist_audio_features <- get_playlist_audio_features(playlist_username, playlist_uris)
playlist_audio_features <- janitor::clean_names(playlist_audio_features)

plot_1 <- ggplot(playlist_audio_features, aes(x = track_popularity)) +
  geom_point(aes(y = energy), color = "#1DB954", alpha = 0.7) + 
  geom_smooth(aes(y = energy), method = "lm", se = FALSE, color = "#1DB954") + 
  geom_point(aes(y = danceability), color = "#191414", alpha = 0.7) +
  geom_smooth(aes(y = danceability), method = "lm", se = FALSE, color = "#191414") + 
  labs(#x = "Track Popularity",
       title = "What makes a song popular?",
       subtitle = "<br>Below are audio feature data for tracks on Spotify's top 
       <br>playlists. How does <span style = 'color:#1DB954;font-family: Gotham-Bold;'>energy</span> or <span style = 'color:#191414;font-family: Gotham-Bold;'>danceability</span> correlate with 
       <br>popularity across different genres? <br>") +
  scale_x_continuous(limits = c(10, 100),
                     breaks = seq(10, 100, by = 10),
                     labels = c(0, seq(20, 100, by = 10)),
                     expand = c(0.01, 0, 0.05, 0)) +
  annotate(geom = "text",
           label = "Today's Top Hits",
           x = 90,
           y = 0.15,
           family = "Gotham-Bold") +
  theme_plot() +
  theme(axis.title = element_blank())

gt <- ggplotGrob(plot_1)

is_xaxis <- which(gt$layout$name == "axis-b")
xaxis <- gt$grobs[[is_xaxis]]

xline <- xaxis$children[[1]]
xline$y <- unit(rep(1, 4), "npc")
xline$x <- unit(c(0, 0.025, 1, 0.075), "npc")
xline$id <- c(1, 1, 2, 2)
xline$arrow <- arrow(angle = 90)
xaxis$children[[1]] <- xline

gt$grobs[[is_xaxis]] <- xaxis

# grid plotting syntax
grid.newpage()
grid.draw(gt) 


# Top Rap
playlist_uris <- gsub(".*:", "", top[2,]$uri, fixed = FALSE)
playlist_audio_features <- get_playlist_audio_features(playlist_username, playlist_uris)
playlist_audio_features <- janitor::clean_names(playlist_audio_features)

plot_2 <- ggplot(playlist_audio_features, aes(x = track_popularity)) +
  geom_point(aes(y = energy), color = "#1DB954", alpha = 0.7) + 
  geom_smooth(aes(y = energy), method = "lm", se = FALSE, color = "#1DB954") + 
  geom_point(aes(y = danceability), color = "#191414", alpha = 0.7) +
  geom_smooth(aes(y = danceability), method = "lm", se = FALSE, color = "#191414") + 
  #labs(x = "Track Popularity") +
  scale_x_continuous(limits = c(50, 100),
                     breaks = seq(50, 100, by = 10),
                     labels = c(0, seq(60, 100, by = 10)),
                     expand = c(0.01, 0, 0.05, 0)) +
  annotate(geom = "text",
           label = "Rap Caviar",
           x = 95,
           y = 0.1,
           family = "Gotham-Bold") +
  theme_plot() +
  theme(axis.title = element_blank())

gt_2<- ggplotGrob(plot_2)

is_xaxis <- which(gt_2$layout$name == "axis-b")
xaxis <- gt_2$grobs[[is_xaxis]]

xline <- xaxis$children[[1]]
xline$y <- unit(rep(1, 4), "npc")
xline$x <- unit(c(0, 0.025, 1, 0.075), "npc")
xline$id <- c(1, 1, 2, 2)
xline$arrow <- arrow(angle = 90)
xaxis$children[[1]] <- xline

gt_2$grobs[[is_xaxis]] <- xaxis

# grid plotting syntax
grid.newpage()
grid.draw(gt_2) 

# Top Rock
playlist_uris <- gsub(".*:", "", top[3,]$uri, fixed = FALSE)
playlist_audio_features <- get_playlist_audio_features(playlist_username, playlist_uris)
playlist_audio_features <- janitor::clean_names(playlist_audio_features)

plot_3 <- ggplot(playlist_audio_features, aes(x = track_popularity)) +
  geom_point(aes(y = energy), color = "#1DB954", alpha = 0.7) + 
  geom_smooth(aes(y = energy), method = "lm", se = FALSE, color = "#1DB954") + 
  geom_point(aes(y = danceability), color = "#191414", alpha = 0.7) +
  geom_smooth(aes(y = danceability), method = "lm", se = FALSE, color = "#191414") + 
  #labs(x = "Track Popularity") +
  scale_x_continuous(limits = c(40, 100),
                     breaks = seq(40, 100, by = 10),
                     labels = c(0, seq(50, 100, by = 10)),
                     expand = c(0.01, 0, 0.05, 0)) +
  annotate(geom = "text",
           label = "Rock This",
           x = 95,
           y = 0.1,
           family = "Gotham-Bold") +
  theme_plot() +
  theme(axis.title = element_blank())
 
gt_3 <- ggplotGrob(plot_3)

is_xaxis <- which(gt_3$layout$name == "axis-b")
xaxis <- gt_3$grobs[[is_xaxis]]

xline <- xaxis$children[[1]]
xline$y <- unit(rep(1, 4), "npc")
xline$x <- unit(c(0, 0.025, 1, 0.075), "npc")
xline$id <- c(1, 1, 2, 2)
xline$arrow <- arrow(angle = 90)
xaxis$children[[1]] <- xline

gt_3$grobs[[is_xaxis]] <- xaxis

# grid plotting syntax
grid.newpage()
grid.draw(gt_3) 

# Top Country
playlist_uris <- gsub(".*:", "", top[5,]$uri, fixed = FALSE)
playlist_audio_features <- get_playlist_audio_features(playlist_username, playlist_uris)
playlist_audio_features <- janitor::clean_names(playlist_audio_features)

plot_4 <- ggplot(playlist_audio_features, aes(x = track_popularity)) +
  geom_point(aes(y = energy), color = "#1DB954", alpha = 0.7) + 
  geom_smooth(aes(y = energy), method = "lm", se = FALSE, color = "#1DB954") + 
  geom_point(aes(y = danceability), color = "#191414", alpha = 0.7) +
  geom_smooth(aes(y = danceability), method = "lm", se = FALSE, color = "#191414") + 
  #labs(x = "Track Popularity") +
  scale_x_continuous(limits = c(50, 100),
                     breaks = seq(50, 100, by = 10),
                     labels = c(0, seq(60, 100, by = 10)),
                     expand = c(0.01, 0, 0.05, 0)) +
  annotate(geom = "text",
           label = "Hot Country",
           x = 95,
           y = 0.1,
           family = "Gotham-Bold") +
  theme_plot() +
  theme(axis.title = element_blank())

gt_4<- ggplotGrob(plot_4)

is_xaxis <- which(gt_4$layout$name == "axis-b")
xaxis <- gt_4$grobs[[is_xaxis]]

xline <- xaxis$children[[1]]
xline$y <- unit(rep(1, 4), "npc")
xline$x <- unit(c(0, 0.025, 1, 0.075), "npc")
xline$id <- c(1, 1, 2, 2)
xline$arrow <- arrow(angle = 90)
xaxis$children[[1]] <- xline

gt_4$grobs[[is_xaxis]] <- xaxis

# grid plotting syntax
grid.newpage()
grid.draw(gt_4) 

# Top Latino
playlist_uris <- gsub(".*:", "", top[6,]$uri, fixed = FALSE)
playlist_audio_features <- get_playlist_audio_features(playlist_username, playlist_uris)
playlist_audio_features <- janitor::clean_names(playlist_audio_features)

plot_5 <- ggplot(playlist_audio_features, aes(x = track_popularity)) +
  geom_point(aes(y = energy), color = "#1DB954", alpha = 0.7) + 
  geom_smooth(aes(y = energy), method = "lm", se = FALSE, color = "#1DB954") + 
  geom_point(aes(y = danceability), color = "#191414", alpha = 0.7) +
  geom_smooth(aes(y = danceability), method = "lm", se = FALSE, color = "#191414") + 
  labs(x = "Track Popularity") +
  scale_x_continuous(limits = c(60, 100),
                     breaks = seq(60, 100, by = 10),
                     labels = c(0, seq(70, 100, by = 10)),
                     expand = c(0.01, 0, 0.05, 0)) +
  annotate(geom = "text",
           label = "Viva Latino",
           x = 97,
           y = 0.1,
           family = "Gotham-Bold") +
  labs(
    caption = "<br>"
  ) +
  theme_plot() 
  

gt_5 <- ggplotGrob(plot_5)

is_xaxis <- which(gt_5$layout$name == "axis-b")
xaxis <- gt_5$grobs[[is_xaxis]]

xline <- xaxis$children[[1]]
xline$y <- unit(rep(1, 4), "npc")
xline$x <- unit(c(0, 0.025, 1, 0.075), "npc")
xline$id <- c(1, 1, 2, 2)
xline$arrow <- arrow(angle = 90)
xaxis$children[[1]] <- xline

gt_5$grobs[[is_xaxis]] <- xaxis

# grid plotting syntax
grid.newpage()
grid.draw(gt_5) 


# Top New
playlist_uris <- gsub(".*:", "", top[12,]$uri, fixed = FALSE)
playlist_audio_features <- get_playlist_audio_features(playlist_username, playlist_uris)
playlist_audio_features <- janitor::clean_names(playlist_audio_features)

plot_6 <- ggplot(playlist_audio_features, aes(x = track_popularity)) +
  geom_point(aes(y = energy), color = "#1DB954", alpha = 0.7) + 
  geom_smooth(aes(y = energy), method = "lm", se = FALSE, color = "#1DB954") + 
  geom_point(aes(y = danceability), color = "#191414", alpha = 0.7) +
  geom_smooth(aes(y = danceability), method = "lm", se = FALSE, color = "#191414") + 
  labs(x = "Track Popularity") +
  scale_x_continuous(limits = c(20, 100),
                     breaks = seq(20, 100, by = 10),
                     labels = c(0, seq(30, 100, by = 10)),
                     expand = c(0.01, 0, 0.05, 0)) +
  annotate(geom = "text",
           label = "New Music Friday",
           x = 91,
           y = 0.1,
           family = "Gotham-Bold") +
  labs(
    caption = "Created by @kllycttn | Data from Spotify"
  ) +
  theme_plot()

gt_6 <- ggplotGrob(plot_6)

is_xaxis <- which(gt_6$layout$name == "axis-b")
xaxis <- gt_6$grobs[[is_xaxis]]

xline <- xaxis$children[[1]]
xline$y <- unit(rep(1, 4), "npc")
xline$x <- unit(c(0, 0.025, 1, 0.075), "npc")
xline$id <- c(1, 1, 2, 2)
xline$arrow <- arrow(angle = 90)
xaxis$children[[1]] <- xline

gt_6$grobs[[is_xaxis]] <- xaxis

# grid plotting syntax
grid.newpage()
grid.draw(gt_6) 

# Plot together
grid.newpage()
plot_combo <- grid.arrange(gt, gt_2, gt_3, gt_4, gt_5, gt_6)

ggsave(plot = plot_combo, here::here("code","2022", "Day13", "day13.png"), width = 12, height = 10, unit = "in", bg = "white")

