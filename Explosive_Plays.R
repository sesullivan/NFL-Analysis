###################################################
# explosive plays

# s. sullivan
# 1/21/2021
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

# connect to SQLite dbo
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
  filter(season == 2020, season_type == "REG") %>%
  collect()

dbDisconnect(connection)


# aggregate data by team
aggOff <- data %>%
  filter(!is.na(epa), play_type %in% c("pass","run")) %>%
  mutate(explosive_play = case_when(play_type == "pass" & yards_gained >= 15 ~ 1,
                                    play_type == "run" & yards_gained >= 10 ~ 1,
                                    TRUE ~ 0)) %>%
  group_by(posteam) %>%
  summarize(
    plays = n(),
    explosive_plays = sum(explosive_play),
    explosive_perc = sum(explosive_play) / n()
  )

aggDef <- data %>%
  filter(!is.na(epa), play_type %in% c("pass","run")) %>%
  mutate(explosive_play = case_when(play_type == "pass" & yards_gained >= 15 ~ 1,
                                    play_type == "run" & yards_gained >= 10 ~ 1,
                                    TRUE ~ 0)) %>%
  group_by(defteam) %>%
  summarize(
    plays = n(),
    explosive_plays = sum(explosive_play),
    explosive_perc = sum(explosive_play) / n()
  )


# load games data
games <- read_csv("http://nflgamedata.com/games.csv")

# calcualte team PPG

# offense
homeScoresOff <- games %>%
  filter(season == 2020, week <= 17) %>%
  select(team = home_team, 
         score = home_score)

awayScoresOff <- games %>%
  filter(season == 2020, week <= 17) %>%
  select(team = away_team, 
         score = away_score)

ppgOff <- 
  rbind(homeScoresOff, awayScoresOff) %>%
  group_by(team) %>%
  summarize(ppg = mean(score))

# defense
homeScoresDef <- games %>%
  filter(season == 2020, week <= 17) %>%
  select(team = home_team, 
         score = away_score)

awayScoresDef <- games %>%
  filter(season == 2020, week <= 17) %>%
  select(team = away_team, 
         score = home_score)

ppgDef <- 
  rbind(homeScoresDef, awayScoresDef) %>%
  group_by(team) %>%
  summarize(ppg = mean(score))  


# join explosive plays and ppg
  
all <- aggOff %>%
  left_join(select(aggDef, defteam, def_explosive_perc = explosive_perc), by = c("posteam" = "defteam")) %>%
  left_join(select(ppgOff, team, off_ppg = ppg), by = c("posteam" = "team")) %>%
  left_join(select(ppgDef, team, def_ppg = ppg), by = c("posteam" = "team")) %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))  

# plot explosive play rate and PPG for offense

all %>%
  ggplot(aes(x = explosive_perc, y = off_ppg)) +
  # add points for the teams with the logos
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  # add a smooth line explosive play rate and PPG
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm', size = 1) +
  #horizontal line with mean PPG
  geom_hline(yintercept = mean(all$off_ppg), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean explosive_perc
  geom_vline(xintercept =  mean(all$explosive_perc), color = "red", linetype = "dashed", alpha=0.5) +
  # adjust axes
  scale_x_continuous(limit = c(.1,.18), breaks = scales::pretty_breaks(n = 4), labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limit = c(15,32), breaks = scales::pretty_breaks(n = 3)) +
  # titles and caption
  labs(x = "Explosive Play Rate",
       y = "Offensive Points per Game",
       title = "2020 Explosive Plays and Offensive Production",
       subtitle = "Explosive Plays are 10yd+ Runs and 15yd+ Passes",
       caption = "Data: @nflfastR & nflgamedata.com | Figure: @Analytics_NYG") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot")


# plot explosive play rate and PPG for defense

all %>%
  ggplot(aes(x = def_explosive_perc, y = def_ppg)) +
  # add points for the teams with the logos
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  # add a smooth line explosive play rate and PPG
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm', size = 1) +
  #horizontal line with mean PPG
  geom_hline(yintercept = mean(all$off_ppg), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean explosive_perc
  geom_vline(xintercept =  mean(all$explosive_perc), color = "red", linetype = "dashed", alpha=0.5) +
  # adjust axes
  scale_x_continuous(limit = c(.1,.18), breaks = scales::pretty_breaks(n = 4), labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limit = c(15,32), breaks = scales::pretty_breaks(n = 3)) +
  # titles and caption
  labs(x = "Explosive Play Rate",
       y = "Offensive Points per Game",
       title = "2020 Explosive Plays and Offensive Production",
       subtitle = "Explosive Plays are 10yd+ Runs and 15yd+ Passes",
       caption = "Data: @nflfastR & nflgamedata.com | Figure: @Analytics_NYG") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot")