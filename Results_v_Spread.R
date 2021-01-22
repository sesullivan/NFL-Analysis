###################################################
# vegas spread results vs. favorite margin of victory

# s. sullivan
# 1/5/2021
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

# load games data
games <- read_csv("http://nflgamedata.com/games.csv")

# find margin of result - spread
ltd <- games %>%
  filter(season >= 2010, week <= 17) %>%
  select(season, game_id, week, result, spread_line, home_team, away_team, home_score, away_score) %>%
  mutate( favorite = if_else(spread_line >= 0, home_team, away_team),
          underdog = if_else(spread_line >= 0, away_team, home_team),
          favorite_score = if_else(spread_line >= 0, home_score, away_score),
          underdog_score = if_else(spread_line >= 0, away_score, home_score),
          favorite_margin = favorite_score - underdog_score,
          favored_by = abs(spread_line),
          covered_by = favorite_margin - favored_by,
          #margin = result - spread_line,
          cat = if_else(season != 2020, "2010-2019","2020"))


ltd %>%
  ggplot() +
  geom_density(aes(x = covered_by, group = cat, fill = cat), alpha = 0.3) +
  # adjust axes
  scale_y_continuous(limit = c(0,0.035), breaks = scales::pretty_breaks(n = 4), expand = c(0,0)) +
  scale_x_continuous(limit = c(-50,50), breaks = scales::pretty_breaks(n = 5), expand = c(0,0)) +
  # titles and caption
  labs(x = "Favorite Margin of Victory Minus Vegas Spread",
       y = "Density",
       title = "Difference Between Favorite Margin of Victory & Vegas Spread",
       subtitle = "NFL Regular Season Games 2010-2020",
       caption = "Data: nflgamedata.com | Figure: @Analytics_NYG") +
  #uses the black and white ggplot theme
  theme_bw() +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.3), 
        plot.subtitle = element_text(hjust = 0.3), 
        legend.title=element_blank(),
        plot.caption.position = "plot")

# get some summary stats
ltd %>%
  group_by(cat) %>%
  summarize(mean = mean(covered_by), sd = sd(covered_by))

ltd %>%
  group_by(season) %>%
  summarize(mean = mean(covered_by), sd = sd(covered_by))

# is this stat sig?
res <- t.test(covered_by ~ cat, data = ltd)
res
# kind of...p = 0.05
