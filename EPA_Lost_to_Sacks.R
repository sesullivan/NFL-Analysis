###################################################
# EPA lost to sacks

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
library(broom)

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
# calculate total epa lost to sacks
###################################################

agg <- data %>%
  filter(sack == 1) %>%
  group_by(posteam) %>%
  summarize(
    total_sack = n(),
    total_epa = sum(epa, na.rm = TRUE),
    total_yards = sum(yards_gained, na.rm = TRUE),
    avg_yards = sum(yards_gained, na.rm = TRUE) / n(),
    avg_epa = sum(epa, na.rm = TRUE) / n()
  ) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

agg %>% view()

###################################################
# graph sacks / epa
###################################################

agg %>%
  ggplot(aes(x = total_sack, y = total_epa)) +
  #add points for the team with the logos
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  # adjust axes
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  #titles and caption
  labs(x = "Total Sacks Allowed",
       y = "Total EPA Lost",
       title = "Total Sacks Allowed v. EPA Lost",
       caption = "Data: @nflfastR | Figure: @Analytics_NYG",
       subtitle = "2020 Weeks 1-11") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot"
  )

agg %>%
  ggplot(aes(y = avg_epa, x = reorder(posteam,avg_epa))) +
  geom_bar(stat = "identity",fill = agg$team_color) +
  # add team logos to end of bars
  geom_image(data = agg, aes(image = team_logo_espn, x = posteam, y = avg_epa, fill = NULL), asp = 16/9) +
  # adjust axes
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  # titles and caption
  labs(x = "",
       y = "Average EPA Lost",
       title = "Average EPA Lost per Sack",
       subtitle = "2020 Weeks 1-11",
       caption = "Data: @nflfastR | Figure: @Analytics_NYG") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot",
        # remove x-axis labels
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())  