###################################################
# offensive yards by down

# s. sullivan
# 10/13/2020
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
# calculate air yards by down
###################################################

agg <- data %>%
  filter(!is.na(down) & down < 4 & !is.na(posteam) &
           wp > .20 & wp < .80 & half_seconds_remaining > 120) %>%
  group_by(posteam, down) %>%
  summarize(
    total_epa = sum(epa, na.rm = T),
    total_plays = n(),
    epa_per_play = sum(epa, na.rm = T) / n(),
    total_yards_gained = sum(yards_gained, na.rm = T),
    yards_per_play = sum(yards_gained, na.rm = T) / n(),
    total_air_yards = sum(air_yards, na.rm = T),
    air_yards_per_play = sum(air_yards, na.rm = T) / n()
  ) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

# get totals for positioning of team logo
totals <- agg %>%
  group_by(posteam) %>%
  summarize(
    total_air_yds = sum(air_yards_per_play)
  ) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) 

###################################################
# plot by team / down
###################################################

agg %>%
  ggplot(aes(y = reorder(posteam, air_yards_per_play), x = air_yards_per_play, fill = as.factor(down))) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity") +
  # change bar colors
  scale_fill_brewer(palette = "YlGnBu") +
  # add team logos to end of bars
  geom_image(data = totals, aes(image = team_logo_espn, x = total_air_yds, y = posteam, fill = NULL)) +
  # adjust axes
  scale_x_continuous(limit = c(0,20), breaks = scales::pretty_breaks(n = 5), expand = c(0,0)) +
  # adjust scale
  guides(fill = guide_legend("Down")) +
  # titles and caption
  labs( x = "Air Yards per Play",
        y = "",
        title = "Air Yards per Play (2020 Weeks 1-5)",
        subtitle = "Win Probability 20-80%, Excluding Final 2 Mins of Each Half",
        caption = "Data: @nflfastR | Analysis: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot")  



