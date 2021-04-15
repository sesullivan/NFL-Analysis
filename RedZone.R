###################################################
# NYG FA additions

# s. sullivan
# 4/15/2021
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
  filter(season >= 2017) %>%
  collect()

dbDisconnect(connection)

###################################################
# 2020 RZ efficiency
###################################################

agg <- data %>%
  filter(season == 2020, season_type == "REG", play_type == "pass", yardline_100 <= 20, !is.na(epa)) %>%
  group_by(posteam) %>%
  summarize(
    epa_pass = sum(epa) / n(),
    tds = length(epa[touchdown == 1])
    ) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))


# 2020 plot
agg %>%
  ggplot(aes(x = tds, y = epa_pass)) +
  #add points for the teams with the logos
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  # adjust axes
  scale_y_continuous(limit = c(-0.75,0.6), breaks = scales::pretty_breaks(n = 5)) +
  scale_x_continuous(limit = c(5,40), breaks = scales::pretty_breaks(n = 4)) +  
  # adjust scale names
  #guides(size = guide_legend("Total Yards Gained")) +
  # titles and caption
  labs(x = "RZ TD Passes",
       y = "EPA per Pass",
       title = "Red Zone TD Passes and EPA",
       subtitle = "2020 Weeks 1-17 | Limited to Red Zone Pass Plays",
       caption = "Data: @nflfastR | Figure: @Analytics_NYG") +
  theme_bw() +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot")


###################################################
# 2020 RZ efficiency
###################################################

players <- data %>%
  filter(season_type == "REG", play_type == "pass", yardline_100 <= 20, !is.na(epa), !is.na(receiver_player_name)) %>%
  group_by(posteam, receiver_player_name) %>%
  summarize(
    epa_pass = sum(epa) / n(),
    total_epa = sum(epa),
    targets = n(),
    tds = length(epa[touchdown == 1])
  ) %>%
  ungroup() %>%
  filter(targets >= 25) %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

# plot
players %>%
  ggplot(aes(x = tds, y = total_epa, size = targets)) +
  geom_point(color = players$team_color, alpha = .6, show.legend = FALSE) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(data = subset(players, receiver_player_name %in% c("K.Golladay","K.Rudolph","D.Adams")), aes(label = receiver_player_name), show.legend = FALSE) +
  # adjust axes
  scale_y_continuous(limit = c(-25,75), breaks = scales::pretty_breaks(n = 5)) +
  scale_x_continuous(limit = c(0,40), breaks = scales::pretty_breaks(n = 4)) +  
  # adjust scale names
  #guides(size = guide_legend("Total Yards Gained")) +
  # titles and caption
  labs(x = "Total RZ Touchdowns",
       y = "Total EPA",
       title = "Red Zone EPA and TDs for Receivers",
       subtitle = "Regular Season 2017-2020 | Limited to Pass Plays | Bubble Size is a Function of Total RZ Targets",
       caption = "Data: @nflfastR | Figure: @Analytics_NYG") +
  theme_bw() +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot")


players %>%
  filter(receiver_player_name == "K.Golladay")

players %>%
  filter(receiver_player_name == "K.Rudolph")
