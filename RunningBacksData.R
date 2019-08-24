# IMPORT NEEDED PACKAGES TO COMPLETE ANALYSIS

library(devtools)
library(nflscrapR)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(stringr)
library(Metrics)

# SET SEED NUMBER TO 69 TO BE ABLE TO REPLICATE RESULTS

set.seed(69)

# CREATE FILE PATHS TO ALL THREE SEASONS OF DATA

file_paths <- list("C:/Users/Brian/Documents/NFL Data/season_2016",
                   "C:/Users/Brian/Documents/NFL Data/season_2017",
                   "C:/Users/Brian/Documents/NFL Data/season_2018")

# IMPORT DATA INTO A LIST

season_data <- map(file_paths, ~ read.csv(.x, stringsAsFactors = FALSE))

# NAME EACH DATA FRAME IN THE LIST TO THE CORRECT SEASON NAME

names(season_data) <- c("season_2016", "season_2017", "season_2018")

# SEPARATE EACH DATAFRAME IN THE LIST INTO ITS OWN DATA FRAME

season_2016 <- season_data[["season_2016"]]
season_2017 <- season_data[["season_2017"]]
season_2018 <- season_data[["season_2018"]]

# CLEAN DATA BY MAKING SURE EVERYTHING HAS THE CORRECT NAME

season_2016$posteam <- str_replace(season_2016$posteam, "JAC", "JAX")

# PUT THE DATA BACK INTO A LIST AND RENAME IT

season_data <- list(season_2016, season_2017, season_2018)

names(season_data) <- c("season_2016", "season_2017", "season_2018")

# CREATE FUNCTIONS TO MANIPULATE THE DATA INTO THE NEEDED FORMAT

get_runplays <- function(x){
  x %>%
    filter(PlayType == 'Run') %>%
      select(qtr,
             yrdline100,
             DefensiveTeam,
             PlayAttempted,
             Yards.Gained,
             Touchdown,
             Rusher,
             Rusher_ID,
             RunLocation,
             RunGap,
             posteam,
             DefensiveTeam) 
}

get_rbs <- function(x){
 x %>%
  group_by(posteam, Rusher) %>%
    filter(!is.na(RunLocation)) %>%
      summarize(rush_attempts = n(),
                rush_yards = sum(Yards.Gained),
                rush_tds = sum(Touchdown),
                longestrun = max(Yards.Gained)) %>%
                  mutate(ypa = rush_yards / rush_attempts) %>%
                    filter(rush_attempts >= 100) %>%
                      arrange(desc(rush_yards), desc(rush_tds))
}

get_passplays <- function(x){
  x %>%
    filter(PlayType == 'Pass') %>%
      select(qtr,
             yrdline100,
             DefensiveTeam,
             Yards.Gained,
             Touchdown,
             Passer,
             Passer_ID,
             PassAttempt,
             PassOutcome,
             PassLength,
             AirYards,
             YardsAfterCatch,
             PassLocation,
             InterceptionThrown,
             Receiver,
             Receiver_ID,
             posteam,
             DefensiveTeam)
  
}

get_receiverstats <- function(x){
  x %>%
    group_by(posteam, Receiver) %>%
      summarize(targets = sum(PassAttempt),
                receptions = sum(PassOutcome == 'Complete'),
                rec_yards = sum(Yards.Gained),
                rec_tds = sum(Touchdown),
                air_yds = sum(AirYards),
                yac = sum(YardsAfterCatch),
                longestreception = max(Yards.Gained))%>%
                  mutate(avg_dot = air_yds / targets,
                         avg_yds_pr = rec_yards / receptions) %>%
                          select(-air_yds) %>%
                            arrange(desc(rec_yards), desc(rec_tds), desc(receptions))
}

get_rz_runs <- function(x){
  x %>%
    filter(yrdline100 <= 20) %>%
    unite("rbid", "Rusher", "posteam", "season", remove = FALSE) %>%
      group_by(rbid) %>%
        filter(!is.na(RunLocation)) %>%
          summarize(rush_attempts = n(),
                    rush_yards = sum(Yards.Gained),
                    rush_tds = sum(Touchdown)) %>%
                      mutate(ypa = rush_yards / rush_attempts) %>%
                        subset(rbid %in% running_backs) %>%
                          arrange(desc(rush_attempts))
}

get_rz_passes <- function(x){
  x %>%
    filter(yrdline100 <= 20) %>%
    unite("rbid", "Receiver", "posteam", "season", remove = FALSE) %>%
      group_by(rbid) %>%
        summarize(targets = sum(PassAttempt),
                  receptions = sum(PassOutcome == 'Complete'),
                  rec_yards = sum(Yards.Gained),
                  rec_tds = sum(Touchdown)) %>%
                    subset(rbid %in% running_backs) %>%
                      arrange(desc(receptions))
}

get_team_runoffense <- function(x){
  x %>%
    group_by(posteam) %>%
      filter(!is.na(RunLocation)) %>%
        summarize(rush_attempts = n(),
                  rush_yards = sum(Yards.Gained),
                  rush_tds = sum(Touchdown)) %>%
                    mutate(ypa = rush_yards / rush_attempts) %>%
                      filter(rush_attempts >= 100) %>%
                        arrange(desc(rush_yards), desc(rush_tds)) %>%
                          mutate(rank = 1:32)
}

get_team_passoffense <- function(x){
  x %>%
    group_by(posteam) %>%
      summarize(attempts = sum(PassAttempt),
                completions = sum(PassOutcome == 'Complete'),
                pass_yards = sum(Yards.Gained),
                pass_tds = sum(Touchdown),
                air_yards = sum(AirYards),
                yac = sum(YardsAfterCatch)) %>%
                  mutate(avg_dot = air_yards / attempts,
                         avg_yds_pa = pass_yards / attempts,
                         avg_yac = yac / completions) %>%
                          select(-air_yards) %>%
                            arrange(desc(pass_yards), desc(pass_tds), desc(completions), desc(attempts)) %>%
                              mutate(rank = 1:32)
}

# ITERATE THESE FUNCTIONS OVER THE LIST TO GET NEW LISTS
# THESE LISTS WILL GIVE ALL RUNPLAYS
# ALL PASS PLAYS
# ALL RUSHING STATS FOR RUNING BACKS
# ALL RECEIVING STATS

run_plays <- map(season_data, get_runplays)

pass_plays <- map(season_data, get_passplays)

rb_rushes <- map(run_plays, get_rbs)

receiving_statsbyplayer <- map(pass_plays, get_receiverstats)

# SEPARATE THE RUNNING BACK LIST INTO INDIVIDUAL DATA FRAMES AND BIND THEM TOGETHER

rb_2016 <- rb_rushes[["season_2016"]] %>% mutate(season = 2016)
rb_2017 <- rb_rushes[["season_2017"]] %>% mutate(season = 2017)
rb_2018 <- rb_rushes[["season_2018"]] %>% mutate(season = 2018)

rb_rush_data <- rbind(rb_2016, rb_2017, rb_2018)

# CONVERT THE SEASON VARIABLE TO A FACTOR

rb_rush_data$season <- as.factor(rb_rush_data$season)

# REPEAT THE SAME STEPS FOR THE RECEIVING STATS

rb_rec_2016 <- receiving_statsbyplayer[["season_2016"]] %>% mutate(season = 2016)
rb_rec_2017 <- receiving_statsbyplayer[["season_2017"]] %>% mutate(season = 2017)
rb_rec_2018 <- receiving_statsbyplayer[["season_2018"]] %>% mutate(season = 2018)

rb_rec_data <- rbind(rb_rec_2016, rb_rec_2017, rb_rec_2018)

rb_rec_data$season <- as.factor(rb_rec_data$season)

colnames(rb_rec_data)[2] <- "Rusher"

# USE UNITE TO CREATE A UNIQUE ID FOR EACH RUNNING BACK IN BOTH THE RUSING AND RECEIVING DATA

rb_rec_data <- unite(rb_rec_data, "rbid", "Rusher", "posteam", "season", remove = FALSE)

rb_rush_data <- unite(rb_rush_data, "rbid", "Rusher", "posteam", "season", remove = FALSE)

# JOIN RUNNING BACK DATA WITH RECEIVING DATA AND CREATE A VARIABLE FOR FANTASY POINTS
# BASED OFF OF THE HALF POINT PER RECEPTION FORMAT

rbs_hppr <- rb_rush_data %>%
              inner_join(rb_rec_data, by = "rbid") %>%
                mutate(tot_yards = rush_yards + rec_yards,
                       tot_tds = rush_tds + rec_tds,
                       fantasy_points = 6 * tot_tds +
                                        0.1 * tot_yards +
                                        0.5 * receptions) %>%
                                          select(-posteam.y, -Rusher.y, -season.y) %>%
                                            arrange(season.x, desc(fantasy_points))

# USE THE TEAM RUSH AND TEAM PASS OFFENSE FUNCTIONS AND TURN THEM INTO DATAFRAMES
# LIKE PREVIOUS DATA FRAMES

team_run_off <- map(run_plays, get_team_runoffense)

names(team_run_off) <- c("season_2016", "season_2017", "season_2018")

team_run_2016 <- team_run_off[["season_2016"]] %>% mutate(season = 2016)
team_run_2017 <- team_run_off[["season_2017"]] %>% mutate(season = 2017)
team_run_2018 <- team_run_off[["season_2018"]] %>% mutate(season = 2018)

team_run_data <- rbind(team_run_2016, team_run_2017,team_run_2018)

team_run_data$season <- as.factor(team_run_data$season)

team_pass_off <- map(pass_plays, get_team_passoffense)

names(team_pass_off) <- c("season_2016", "season_2017", "season_2018")

team_pass_2016 <- team_pass_off[["season_2016"]] %>% mutate(season = 2016)
team_pass_2017 <- team_pass_off[["season_2017"]] %>% mutate(season = 2017)
team_pass_2018 <- team_pass_off[["season_2018"]] %>% mutate(season = 2018)

team_pass_data <- rbind(team_pass_2016, team_pass_2017,team_pass_2018)

team_pass_data$season <- as.factor(team_pass_data$season)

team_run_data <- unite(team_run_data, "teamid", "posteam", "season", remove = FALSE)

team_pass_data <- unite(team_pass_data, "teamid", "posteam", "season", remove = FALSE)

# PULL EACH DATA FRAME FROM THE LIST AND ADD A SEASON COLUMN FOR BOTH THE RUN PLAYS
# AND THE PASS PLAYS THEN PUT THEM BACK INTO A LIST

run_plays_2016 <- run_plays[["season_2016"]] %>% mutate(season = 2016)
run_plays_2017 <- run_plays[["season_2017"]] %>% mutate(season = 2017)
run_plays_2018 <- run_plays[["season_2018"]] %>% mutate(season = 2018)

run_plays <- list(run_plays_2016, run_plays_2017, run_plays_2018)

pass_plays_2016 <- pass_plays[["season_2016"]] %>% mutate(season = 2016)
pass_plays_2017 <- pass_plays[["season_2017"]] %>% mutate(season = 2017)
pass_plays_2018 <- pass_plays[["season_2018"]] %>% mutate(season = 2018)

pass_plays <- list(pass_plays_2016, pass_plays_2017, pass_plays_2018)

#DEFINE A RUNNING BACKS VECTOR WITH ALL THE NAMES OF THE RUNNING BACKS

running_backs <- rbs_hppr$rbid

# ITERATE FUNCTIONS AGAIN OVER ORIGINAL DATAFRAME TO GENERATE REDZONE RUNNING BACK DATA
# GENERATE BOTH RUSHING AND RECEIVING REDZONE DATA

rz_rushes_byplayer <- map(run_plays, get_rz_runs)

rz_targets_byplayer <- map(pass_plays, get_rz_passes)

# RENAME AND EXPORT OUT THE DATAFRAMES FROM THE LISTS
# INTO INDVIDUAL DATAFRAMES AND BIND THEM TOGETHER

names(rz_rushes_byplayer) <- c("season_2016", "season_2017", "season_2018")

names(rz_targets_byplayer) <- c("season_2016", "season_2017", "season_2018")

rb_rz_rush_2016 <- rz_rushes_byplayer[["season_2016"]]
rb_rz_rush_2017 <- rz_rushes_byplayer[["season_2017"]]
rb_rz_rush_2018 <- rz_rushes_byplayer[["season_2018"]]

rb_rz_rush_data <- rbind(rb_rz_rush_2016, rb_rz_rush_2017, rb_rz_rush_2018)

rb_rz_rec_2016 <- rz_targets_byplayer[["season_2016"]]
rb_rz_rec_2017 <- rz_targets_byplayer[["season_2017"]]
rb_rz_rec_2018 <- rz_targets_byplayer[["season_2018"]]

rb_rz_rec_data <- rbind(rb_rz_rec_2016, rb_rz_rec_2017, rb_rz_rec_2018)

# JOIN THE TWO NEW DATAFRAMES TOGETHER BY RBID

rz_rb <- inner_join(rb_rz_rush_data, rb_rz_rec_data, by = "rbid")

# JOIN BOTH THE TEAM RUN AND TEAM PASS DATA FRAMES INTO A SINGLE DATA FRAME
# THEN CREATE TOT TD TOT YARDS VARIABLES

team_offense <- inner_join(team_run_data, team_pass_data, by = "teamid") %>%
                  mutate(tot_yards = rush_yards + pass_yards,
                         tot_tds = rush_tds + pass_tds) %>%
                          arrange(desc(tot_yards), desc(tot_tds)) %>%
                            select(-season.x, -posteam.x)

# JOIN THE TEAM OFFENSE DATA WITH THE RUNNING BACK DATA TO CREATE
# A FINAL DATAFRAME WITH ALL OF THE NEEDED DATA FOR MODELING RB STATISTICS

running_backs_data <- inner_join(rbs_hppr, rz_rb, by = "rbid") %>%
                        unite("teamid", "posteam.x", "season.x", remove = TRUE) %>%
                          left_join(team_offense, by = "teamid")

# CREATE A VECTOR WITH ALL OF THE NAMES FOR THE RUNNING BACKS COLUMN

running_backs_names <- c("rbid",
                         "teamid",
                         "runningback",
                         "rbtotattempts",
                         "rbtotrushyards",
                         "rbtotrushtds",
                         "longestrun",
                         "rbtotypa",
                         "tottargets",
                         "totreceptions",
                         "totrecyards",
                         "totrectds",
                         "rbtotyac",
                         "longestreception",
                         "rbavgdot",
                         "rbavgyardspr",
                         "rbtotyards",
                         "rbtottds",
                         "fantasypoints",
                         "rzrushattempts",
                         "rzrushyards",
                         "rzrushtds",
                         "rzypa",
                         "rztargets",
                         "rzreceptions",
                         "rzrecyards",
                         "rzrectds",
                         "teamrushattempts",
                         "teamrushyards",
                         "teamrushtds",
                         "teamypa",
                         "teamrushrank",
                         "team",
                         "teampassattempts",
                         "teamcompletions",
                         "teampassyards",
                         "teampasstds",
                         "teamyac",
                         "teamavgdot",
                         "teamavgydspa",
                         "teamavgyac",
                         "teampassrank",
                         "season",
                         "teamtotyards",
                         "teamtottds")

# CHANGE THE COLUMN NAMES OF THE RUNNING BACKS DATAFRAME USING THE PREVIOUSLY
# CREATED VECTOR

names(running_backs_data) <- running_backs_names

# EXPORT FINAL DATA FRAME TO A CSV FILE

write.csv(running_backs_data, "C:/Users/Brian/Documents/NFL Data/running_backs_data.csv")
