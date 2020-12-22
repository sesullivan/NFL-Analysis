###################################################
# nyg pass frequency past sticks

# s. sullivan
# 12/15/2020
###################################################

###################################################
# install packages
###################################################

# devtools::install_github("mrcaseb/nflfastR")

library(dplyr)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(DBI)
library(RSQLite)
library(reshape2)

###################################################
# connect to SQLite dbo
###################################################


update_db()
connection <- dbConnect(SQLite(), "./pbp_db")
connection

dbListTables(connection)

pbp_db <- tbl(connection, "nflfastR_pbp")

pbp_db %>%
  group_by(season) %>%
  summarize(n=n())

# limit to the seasons we want
data <- pbp_db %>%
  filter(season == 2020) %>%
  collect()

dbDisconnect(connection)

###################################################
# plot air yards / yards past sticks
###################################################

nyg <- data %>%
  filter(posteam == "NYG" & play_type == "pass" & !is.na(air_yards) & !is.na(ydstogo) & sack == 0 & !is.na(receiver_player_name)) %>%
  mutate(down_type = if_else(down %in% c(1,2),"Early","Late"),
         yards_past = air_yards - ydstogo) %>%
  group_by(defteam, week) %>%
  summarize(total_passes = n(),
            avg_air = sum(air_yards) / n(),
            avg_past = sum(yards_past) / n()) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))


nyg %>%
  ggplot(aes(x = avg_past, y = avg_air)) +
  geom_point(size = nyg$total_passes/5, color =nyg$team_color, alpha = 0.6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=paste(defteam,":Week",week))) +
  # adjust axes
  scale_y_continuous(limit = c(5,12), breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(limit = c(-4,2), breaks = scales::pretty_breaks(n = 6)) +
  # horizontal line with mea
  geom_hline(yintercept = mean(nyg$avg_air), color = "red", linetype = "dashed", alpha=0.5) +
  # vertical line with mean
  geom_vline(xintercept =  mean(nyg$avg_past), color = "red", linetype = "dashed", alpha=0.5) +  
  # titles and caption
  labs(x = "Average Intended Yards Past Sticks",
       y = "Average Intended Air Yards",
       title = "NY Giants Intended Pass Distance",
       subtitle = "2020 Weeks 1-15, All Pass Attempts Excluding Sacks & Throwaways",
       caption = "Data: @nflfastR | Figure: @Analytics_NYG") +
  #uses the black and white ggplot theme
  theme_bw() +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

