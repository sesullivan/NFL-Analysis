###################################################
# postseason team tiers

# s. sullivan
# 12/29/2020
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

# create lu for postseason teams
playoff_teams <- as_tibble(pbp_db %>%
  filter(season_type == "POST" & posteam != "" & !is.na(posteam) & season >= 2010) %>%
  distinct(posteam, season)
  )

# limit to the regular seasons we want
data <- pbp_db %>%
  filter(season >= 2010 & season_type == "REG") %>%
  collect()

dbDisconnect(connection)

###################################################
# limit to postseason teams
###################################################

offense <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa)) %>%
  inner_join(playoff_teams, by = c("posteam","season")) %>%
  group_by(posteam, season) %>%
  summarize(off_epa = mean(epa)) %>%
  ungroup()

defense <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa)) %>%
  inner_join(playoff_teams, by = c("defteam" = "posteam","season")) %>%
  group_by(defteam, season) %>%
  summarize(def_epa = mean(epa)) %>%
  ungroup()

all_plays <- offense %>%
  left_join(defense, by = c("posteam" = "defteam","season")) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

bad <- all_plays %>%
  filter((posteam == "SEA" & season == 2010) | (posteam == "CAR" & season == 2014))


###################################################
# calculate for NFCE teams
###################################################

offenseNFCE <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa), season == 2020, posteam %in% c("NYG","DAL","WAS")) %>%
  group_by(posteam, season) %>%
  summarize(off_epa = mean(epa)) %>%
  ungroup()

defenseNFCE <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa), season == 2020, defteam %in% c("NYG","DAL","WAS")) %>%
  group_by(defteam, season) %>%
  summarize(def_epa = mean(epa)) %>%
  ungroup()

all_playsNFCE <- offenseNFCE %>%
  left_join(defenseNFCE, by = c("posteam" = "defteam","season")) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

###################################################
# plot postseason / NFCE 2020
###################################################

all_plays %>%
  ggplot(aes(x = off_epa, y = def_epa)) +
  # all postseason teams
  geom_point(color = "gray", alpha = 0.6, size = 5) +
  # NFCE teams
  geom_point(data = all_playsNFCE, color = all_playsNFCE$team_color, alpha = 0.6, size = 5) +
  # other bad teams
  geom_point(data = bad, color = bad$team_color, alpha = 0.6, size = 5) +
  # add NFCE names using ggrepel
  geom_text_repel(data = all_playsNFCE, aes(label = paste(posteam,season)), show.legend = FALSE) +
  # add names for some other bad teams using ggrepel
  geom_text_repel(data = bad, aes(label = paste(posteam,season)), show.legend = FALSE) +
  # adjust axes
  scale_y_reverse(limit = c(0.15,-0.25), breaks = scales::pretty_breaks(n = 4)) +
  scale_x_continuous(limit = c(-0.15,0.25), breaks = scales::pretty_breaks(n = 4)) +
  # horizontal line with mean defense
  geom_hline(yintercept = mean(all_plays$def_epa), color = "red", linetype = "dashed", alpha=0.5) +
  # vertical line with mean offense
  geom_vline(xintercept =  mean(all_plays$off_epa), color = "red", linetype = "dashed", alpha=0.5) + 
  # titles and caption
  labs(x = "Offensive EPA per Play",
     y = "Defensive EPA per Play",
     title = "Regular Season EPA per Play for Postseason Teams",
     subtitle = "2010-2019, Plus Potential 2020 NFCE Champions",
     caption = "Data: @nflfastR | Figure: @Analytics_NYG") +
  theme_bw() +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot")

