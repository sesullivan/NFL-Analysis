###################################################
# defensive EPA by down

# s. sullivan
# 10/8/2020
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
# calculate defense epa allowed by down
###################################################

agg <- data %>%
  filter(!is.na(down) & down < 4 & !is.na(defteam)) %>%
  group_by(defteam, down) %>%
  summarize(
    total_epa = sum(epa, na.rm = T),
    total_plays = n(),
    epa_per_play = sum(epa, na.rm = T) / n()
  ) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))

nyg <- agg %>%
  filter(defteam == "NYG")

###################################################
# plot by team / down
###################################################

agg %>%
  ggplot(aes(x = down, y = epa_per_play)) +
  geom_point(color = agg$team_color, alpha = .6, size = 5, position = position_jitter(width = 0.02, height = 0)) +
  # add line connecting NYG dots
  geom_path(data = nyg, size = 1, position = position_jitter(width = 0.02, height = 0)) +
  # adjust axes
  scale_y_continuous(limit = c(-0.8,0.8), breaks = scales::pretty_breaks(n = 7)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2)) +
  # titles and caption
  labs( x = "Down",
        y = "EPA per Play Allowed",
        title = "NY Giants Defensive Efficiency by Down",
        subtitle = "2020 Weeks 1-4",
        caption = "Data: @nflfastR | Analysis: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot")  



