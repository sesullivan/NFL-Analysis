###################################################
# rolling EPA for QBs

# s. sullivan
# 11/5/2020
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
  filter(season >= 2015) %>%
  collect()

dbDisconnect(connection)

###################################################
# calculate rolling EPA by QB 
###################################################

# pull roster info for headshots
rosters <- fast_scraper_roster(c(2015,2016,2017,2018,2019))

player_lu <- rosters %>%
  filter(position == "QB") %>%
  mutate(player = paste0(substr(first_name, 1, 1), ".",last_name)) %>%
  distinct(headshot_url, player)

qbs <- data %>%
  filter(passer %in% c("J.Winston","M.Mariota","J.Goff","C.Wentz","D.Prescott","M.Trubisky",
                       "P.Mahomes","D.Watson","B.Mayfield","S.Darnold","J.Allen","L.Jackson",
                       "K.Murray","D.Jones","D.Haskins","D.Lock")
         & !is.na(qb_epa)) %>%
  group_by(posteam, passer, season, week) %>%
  summarize(week_epa = sum(qb_epa),
            total_plays = n()) %>%
  ungroup() %>%
  filter(total_plays >= 5) %>% # excludes games where the QB didn't really play for whatever reason (e.g. Jones Week 1 2019)
  group_by(passer) %>%
  mutate(game_num = order(season, week, decreasing = FALSE)) %>%
  ungroup()

# create dummy df of 0,0 values for plotting
passersDf = unique(data.frame(posteam = qbs$posteam, passer = qbs$passer, season = 0, week = 0, game_num = 0, week_epa = 0))
  
rolling_epa <- qbs %>%
  bind_rows(passersDf)  %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) %>%
  left_join(player_lu, by = c('passer' = 'player')) %>%
  arrange(passer, game_num) %>%
  group_by(passer) %>%
  mutate(cum_epa = cumsum(week_epa)) %>%
  ungroup()  

# get last value for data labels
data_ends <- rolling_epa %>%
  group_by(passer) %>%
  filter(game_num == max(game_num)) %>%
  ungroup()

# look at first 28 starts (Jones after 2020 season)
data_28 <- rolling_epa %>%
  group_by(passer) %>%
  filter(game_num <= 28) %>%
  ungroup() 

data_end_28 <- data_28 %>%
  group_by(passer) %>%
  filter(game_num == max(game_num)) %>%
  ungroup() 


###################################################
# graph EPA by game number
###################################################

rolling_epa %>%
  ggplot(aes(x = game_num, y = cum_epa, group = passer)) +
  geom_line(color = rolling_epa$team_color, size = 1) +
  geom_text_repel(aes(label = passer), data = data_ends, size = 5) +
  # adjust axes
  scale_y_continuous(limit = c(-100,650), expand = c(0,0), breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(limit = c(0,80), expand = c(0,0), breaks = scales::pretty_breaks(n = 4)) +  
  # titles and caption
  labs(x = "Career Game Number",
     y = "Cumulative EPA",
     title = "Cumulative EPA by Game Number",
     subtitle = "Includes Passes, Rushes, and Penalties for QBs Drafted 2015-2019",
     caption = "Data: @nflfastR | Figure: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


data_28 %>%
  ggplot(aes(x = game_num, y = cum_epa, group = passer)) +
  geom_line(color = data_28$team_color, size = 1) +
  facet_wrap(~passer) +
  geom_image(x = 5, y = 300, aes(image = headshot_url), size = 0.3) +
  # adjust axes
  scale_y_continuous(limit = c(-100,400), expand = c(0,0), breaks = scales::pretty_breaks(n = 3)) +
  scale_x_continuous(limit = c(0,30), expand = c(0,0), breaks = scales::pretty_breaks(n = 3)) +  
  # titles and caption
  labs(x = "Career Game Number",
       y = "Cumulative EPA",
       title = "Cumulative EPA by Game Number (First 28 Games of Career)",
       subtitle = "Includes Passes, Rushes, and Penalties for QBs Drafted 2015-2019",
       caption = "Data: @nflfastR | Figure: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

