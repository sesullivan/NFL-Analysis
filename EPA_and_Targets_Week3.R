###################################################
# daniel jones target/EPA for 2020

# s. sullivan
# 9/28/2020
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
# limit data to giants / jones
###################################################

nyg <- data %>%
  filter(posteam == "NYG" & play_type == "pass" & !is.na(receiver_player_name)) %>%
  group_by(receiver_player_name) %>%
  summarize(
    total_epa = sum(epa, na.rm = T),
    total_air_yards = sum(air_yards, na.rm = T),
    total_yac = sum(yards_after_catch, na.rm = T),
    total_yards_gained = sum(yards_gained, na.rm = T),
    total_completed_air_yards = sum(yards_gained, na.rm = T) - sum(yards_after_catch, na.rm = T),
    total_targets = n()) %>%
  ungroup() %>%
  mutate(player_pos = case_when(
                      receiver_player_name %in% c("C.Board","S.Shepard","D.Ratley","D.Slayton","G.Tate") ~ "WR",
                      receiver_player_name %in% c("D.Lewis","S.Barkley","W.Gallman") ~ "RB",
                      receiver_player_name %in% c("E.Engram","K.Smith","L.Toilolo") ~ "TE"),
        player_pos = factor(player_pos, levels = c("WR","RB","TE"))
  )

###################################################
# plot EPA and yards
###################################################

nyg %>%
  ggplot(aes(x = total_targets, y = total_epa, color = player_pos, size = total_yards_gained)) +
  geom_point() +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label = receiver_player_name)) +
  # adjust axes
  scale_y_continuous(limit = c(-12,6), breaks = scales::pretty_breaks(n = 5)) +
  scale_x_continuous(limit = c(0,20), breaks = scales::pretty_breaks(n = 5)) +  
  # adjust scale names
  guides(col = guide_legend("Player Position"),
         size = guide_legend("Total Yards Gained")) +
  # titles and caption
  labs(x = "Total Targets",
       y = "Total EPA",
       title = "Total Targets v. Total EPA for NYG Receivers",
       subtitle = "2020 Weeks 1-2",
       caption = "Data: @nflfastR, Analysis: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot")

###################################################
# bar chart of air yards / YAC
###################################################

# melt to create variable name as yard type
nyg_melt <- nyg %>%
  filter(total_yards_gained > 0) %>%
  melt(id.vars = "receiver_player_name", measure.vars = c("total_completed_air_yards","total_yac"), variable.name = "type", value.name = "yards")

nyg_melt %>%
  ggplot(aes(x = reorder(receiver_player_name, -yards), y = yards, fill = type)) +
  geom_bar(stat = "identity") +
  # adjust axes
  scale_y_continuous(expand = c(0,0), limits = c(0,150), breaks = scales::pretty_breaks(n = 4)) +
  # titles and caption
  labs(fill = "Yard Type", values = c("Air Yards","YAC"),
       x = "Player",
       y = "Total Yards",
       title = "Air Yards v. Yards After Catch for NYG Receivers",
       subtitle = "2020 Weeks 1-2; Limited to Completed Passes",
       caption = "Data: @nflfastR, Analysis: @Analytics_NYG") +
  # names of categories
  scale_fill_manual(labels = c("Air Yards","YAC"), values = c("light blue","dark blue")) +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.3), plot.subtitle = element_text(hjust = 0.3),
  # adjust x axis labels
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.caption.position = "plot"
        )



