###################################################
# analyze first year v. second year for QB's

# s. sullivan
# 9/16/2020
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
  filter(season >= 2010) %>%
  collect()

dbDisconnect(connection)

###################################################
# find first / second season for each QB, min 200 attempts
###################################################

yr_lu <- data %>%
  select(passer, season) %>%
  group_by(passer, season) %>%
  summarize(total_attempts = n()) %>%
  filter(total_attempts >= 200) %>%
  summarize(first_szn = min(season)) %>%
  filter(first_szn > 2011) %>% #this does exclude 2011 rookies
  mutate(second_szn = first_szn + 1) %>%
  ungroup()


year_1 <- data %>%
  group_by(season, passer, team = posteam) %>%
  summarize(
    epa = mean(qb_epa, na.rm = T),
    cpoe = mean(cpoe, na.rm = T),
    avg_air_yd = mean(air_yards, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
  ) %>%
  inner_join(yr_lu, by = c("passer","season" = "first_szn"))


year_2 <- data %>%
  group_by(season, passer, team = posteam) %>%
  summarize(
    epa = mean(qb_epa, na.rm = T),
    cpoe = mean(cpoe, na.rm = T),
    avg_air_yd = mean(air_yards, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
  ) %>%
  inner_join(yr_lu, by = c("passer","season" = "second_szn"))


# join first/second year into the same table, add team colors
both_yrs <- year_1 %>%
  left_join(year_2, by = "passer", suffix = c("_first","_second")) %>%
  filter(team_first == team_second & n_dropbacks_first >= 200) %>% #taking out any team change a first year QB had (i.e. Rosen)
  left_join(teams_colors_logos, by = c('team_first' = 'team_abbr'))

# remove any player without at least 200 dropbacks in both first/second seasons
both_yrs_ltd <- both_yrs %>%
  filter(n_dropbacks_first >= 200 & n_dropbacks_second >= 200)

###################################################
# plot first year EPA/CPOE
###################################################

both_yrs %>%
  ggplot(aes(x = cpoe_first, y = epa_first)) +
  geom_point(color = both_yrs$team_color, alpha = .6, size = 6) +
  # add emphasis for DJ
  geom_point(data = both_yrs[both_yrs$passer == "D.Jones",],pch=21, fill=NA, size=8, colour="red", stroke=2) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=passer)) +
  # adjust axes3
  scale_y_continuous(limit = c(-15,10), breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(limit = c(-0.4,0.4), breaks = scales::pretty_breaks(n = 10)) +
  # horizontal line with mean second year EPA
  geom_hline(yintercept = mean(both_yrs_ltd$epa_first), color = "red", linetype = "dashed", alpha=0.5) +
  # vertical line with mean first year EPA
  geom_vline(xintercept =  mean(both_yrs_ltd$cpoe_first), color = "red", linetype = "dashed", alpha=0.5) +  
  #add a smooth line fitting year1 + year2
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  # titles and caption
  labs(x = "First Year CPOE",
       y = "First Year EPA per Play",
       title = "Rookie QB EPA v. CPOE (2012-2019)",
       subtitle = "Includes Passes, Rushes, and Penalties for QBs with 200+ Dropbacks in First Season",
       caption = "Data: @nflfastR, Analysis: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

###################################################
# plot first v. second year EPA
###################################################

both_yrs_ltd %>%
  ggplot(aes(x = epa_first, y = epa_second)) +
  geom_point(color = both_yrs_ltd$team_color, alpha = .6, size = 6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=passer)) +
  # adjust axes3
  scale_y_continuous(limit = c(-0.4,0.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(limit = c(-0.4,0.4), breaks = scales::pretty_breaks(n = 10)) +
  # horizontal line with mean second year EPA
  geom_hline(yintercept = mean(both_yrs_ltd$epa_second), color = "red", linetype = "dashed", alpha=0.5) +
  # vertical line with mean first year EPA
  geom_vline(xintercept =  mean(both_yrs_ltd$epa_first), color = "red", linetype = "dashed", alpha=0.5) +  
  #add a smooth line fitting year1 + year2
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  # titles and caption
  labs(x = "First Year EPA per play",
       y = "Second Year EPA per play",
       title = "First Year v. Second Year QB EPA (2012-2019)",
       subtitle = "Includes Passes, Rushes, and Penalties for QBs with 200+ Dropbacks in Both Seasons",
       caption = "Data: @nflfastR, Analysis: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

###################################################
# plot first v. second year CPOE
###################################################

both_yrs_ltd %>%
  ggplot(aes(x = cpoe_first, y = cpoe_second)) +
  geom_point(color = both_yrs_ltd$team_color, alpha = .6, size = 6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=passer)) +
  # adjust axes3
  scale_y_continuous(limit = c(-12,8), breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(limit = c(-12,8), breaks = scales::pretty_breaks(n = 10)) +
  # horizontal line with mean second year EPA
  geom_hline(yintercept = mean(both_yrs_ltd$cpoe_second), color = "red", linetype = "dashed", alpha=0.5) +
  # vertical line with mean first year EPA
  geom_vline(xintercept =  mean(both_yrs_ltd$cpoe_first), color = "red", linetype = "dashed", alpha=0.5) +  
  #add a smooth line fitting year1 + year2
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  # titles and caption
  labs(x = "First Year CPOE",
       y = "Second Year CPOE",
       title = "First Year v. Second Year QB CPOE (2012-2019)",
       subtitle = "Includes QBs with 200+ Dropbacks in Both Seasons",
       caption = "Data: @nflfastR, Analysis: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

###################################################
# investigate if this is linear over time, i.e. do QB's keep improving 
###################################################

year_rank <- data %>%
  group_by(season, passer, team = posteam) %>%
  summarize(
    epa = mean(qb_epa, na.rm = T),
    cpoe = mean(cpoe, na.rm = T),
    avg_air_yd = mean(air_yards, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
  ) %>%
  inner_join(yr_lu, by = "passer") %>%
  filter(n_dropbacks >= 200) %>%
  ungroup() %>%
  group_by(passer, team) %>%
  mutate(season_rank = order(season, decreasing = FALSE)) %>%
  ungroup()

# plot EPA over time
year_rank %>%
  filter(season_rank <= 6) %>% #only one QB drafted since 2012 has played 7+ seasons (Russ)
  ggplot(aes(x = season_rank, y = epa)) +
  # jitter points to see variation
  geom_jitter() +
  # adjust axes
  scale_y_continuous(limit = c(-0.5,0.5), breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  # add median for each year
  stat_summary(fun.y=mean, geom="point", shape=19,
               size=3, color="red") +
  # titles and caption
  labs(x = "QB Season",
       y = "Average EPA per Play",
       title = "Average QB EPA by QB's Season in NFL (2012-2019)",
       subtitle = "Includes Passes, Rushes, and Penalties for QBs with 200+ Dropbacks",
       caption = "Data: @nflfastR, Analysis: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

  # note: this first year EPA is a bit lower than the first v. second year chart. this is because year 1 here includes players with only 1 season 


# plot CPOE over time
year_rank %>%
  filter(season_rank <= 6) %>% #only one QB drafted since 2012 has played 7+ seasons (Russ)
  ggplot(aes(x = season_rank, y = cpoe)) +
  # jitter points to see variation
  geom_jitter() +
  # adjust axes
  scale_y_continuous(limit = c(-10,10), breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  # add median for each year
  stat_summary(fun.y=median, geom="point", shape=19,
               size=3, color="red") +
  # titles and caption
  labs(x = "QB Season",
       y = "Average CPOE",
       title = "Average CPOE by QB's Season in NFL (2012-2019)",
       subtitle = "QBs with 200+ Dropbacks",
       caption = "Data: @nflfastR, Analysis: @Analytics_NYG") +
  # center title / subtitle
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


