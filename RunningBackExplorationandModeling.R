# IMPORT NEEDED PACKAGES TO COMPLETE ANALYSIS

library(devtools)
library(nflscrapR)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(stringr)
library(Metrics)

# SET SEED TO NUMBER 69 TO BE ABLE REPLICATE RESULTS

set.seed(69)

# IMPORT RUNNING BACKS DATA FROM CSV FILE

running_backs_data <- read.csv("C:/Users/Brian/Documents/NFL Data/running_backs_data.csv")

# CREATE SUMMARY STATISTICS FOR EACH VARIABLE

summary_rb <- map(running_backs_data, summary)

# CREATE BOXPLOTS FOR EACH VARIABLE VARIABLE

boxplot(running_backs_data$rbtotattempts,
        running_backs_data$rbtotrushyards,
        running_backs_data$rbtotrushtds,
        running_backs_data$longestrun,
        running_backs_data$ypa,
	  running_backs_data$tottargets,
	  running_backs_data$totreceptions,
	  running_backs_data$totrecyards,
	  running_backs_data$totrectds)

# CREATE HISTOGRAMS FOR EACH VARIABLE LOOKING TO PREDICT

hist(running_backs_data$rbtotrushyards, breaks = 10)

hist(running_backs_data$rbtotrushtds, breaks = 10)

hist(running_backs_data$totreceptions, breaks = 10)

hist(running_backs_data$totrecyards, breaks = 10)

hist(running_backs_data$totrectds, breaks = 10)

# CALCULATE PERCENT OF RUNNINGBACKS THAT RUSH FOR 1000 + YARDS

percent_thousand_yard_rusher <- mean(running_backs_data$rbtotrushyards >= 1000)

# CALCULATE PERCENT OF RUNNING BACKS THAT RUSH FOR 10 + TDS

percent_10plustds <- mean(running_backs_data$rbtotrushtds >= 10)

# CALCULATE PERCENT OF RUNNING BACKS THAT CATCH 50 + PASSES

percent_50pluscatches <- mean(running_backs_data$totreceptions >= 50)

# CALCULATE PERCENT OF RUNNING BACKS THAT HAVE 400 + RECEIVING YARDS

percent_400plusrecyards <- mean(running_backs_data$totrecyards >= 400)

# CALCULATE PERCENT OF RUNNING BACKS THAT CATCH 3 + TDS

percent_3plusrectds <- mean(running_backs_data$totrectds >= 3)

# CREATE SCATTER PLOTS FOR EACH VARIABLE

par(mfcol = c(3,4))

plot(running_backs_data$rbtotattempts, running_backs_data$rbtotrushyards)
plot(running_backs_data$longestrun, running_backs_data$rbtotrushyards)
plot(running_backs_data$rbtotrushtds, running_backs_data$rbtotrushyards)
plot(running_backs_data$teamrushyards, running_backs_data$rbtotrushyards)
plot(running_backs_data$teamrushattempts, running_backs_data$rbtotrushyards)
plot(running_backs_data$teamtotyards, running_backs_data$rbtotrushyards)
plot(running_backs_data$teamrushrank, running_backs_data$rbtotrushyards)
plot(running_backs_data$teamrushrank, running_backs_data$rbtotattempts)
plot(running_backs_data$teamrushrank, running_backs_data$longestrun)
plot(running_backs_data$longestrun, running_backs_data$rbtotattempts)

# CREATE CORRELATION MATRIX

correlation_rb <- running_backs_data %>%
                    select(-rbid, -teamid, -runningback, -team, - season) %>%
                      cor()

# CREATE TRAINING AND TEST DATA SETS TO TEST MODEL ACCURACY

smp_size <- floor(0.8 * nrow(running_backs_data))

train_ind <- sample(seq_len(nrow(running_backs_data)), size = smp_size)

train <- running_backs_data[train_ind, ]
test <- running_backs_data[-train_ind, ]

# CREATE MODEL TO PREDICT RUSHING YARDS ON TRAINING DATA

rb_rush_yds_train_lm <- lm(log(rbtotrushyards) ~ log(rbtotattempts) +
                                                 log(longestrun) + 
                                                 teamrushrank, train)

# ALL P-VALUES LESS THAN .05
# R-SQUARED .893 AND ADJUSTED R-SQUARED OF .890
# F-STATISTIC 258.7

# USE MODEL TO CREATE PREDICTIONS ON TEST SET

test$predictionsrushyards <- predict(rb_rush_yds_train_lm, test)

test$predictionsrushyards <- exp(test$predictionsrushyards)

# DETERMINE MAE AND RMSE ON PREDICTIONS

rmse(test$rbtotrushyards, test$predictionsrushyards)

mae(test$rbtotrushyards, test$predictionsrushyards)

# RMSE 108.87

# MAE 89.20

# CREATE MODEL TO PREDICT RUSHING TDS ON TRAINING DATA

rb_rush_tds_train_lm <- lm(rbtotrushtds ~ rzrushattempts + teamrushtds, train)

summary(rb_rush_tds_train_lm)

# ALL P-VALUES LESS THAN .05
# R-SQUARED .714
# ADJUSTED R-SQUARED .708

# USE MODEL TO CREATE PREDICTIONS ON TEST SET

test$predictionsrushtds <- predict(rb_rush_tds_train_lm, test)

# DETERMINE RMSE AND MAE ON PREDICTIONS

rmse(test$rbtotrushtds, test$predictionsrushtds)

mae(test$rbtotrushtds, test$predictionsrushtds)

# RMSE 1.62
# MAE 1.31

# CREATE A MODEL TO PREDICT TOTAL RECEPTIONS ON TRAINING DATA

rb_catch_train_lm <- lm(totreceptions ~ log(tottargets), train)

summary(rb_catch_train_lm)

# ALL P-VALUES LESS THAN .05
# R-SQUARED .796
# ADJUSTED R-SQUARED .794

# USE MODEL TO CREATE PREDICTIONS ON TEST SET

test$predictionsreceptions <- predict(rb_catch_train_lm, test)

# DETERMINE RMSE AND MAE ON PREDICTIONS

rmse(test$totreceptions, test$predictionsreceptions)

mae(test$totreceptions, test$predictionsreceptions)

# RMSE 10.40
# MAE 6.13

# CREATE A MODEL TO PREDICT TOTAL RECEIVING TDS ON TRAINING DATA

rb_rec_tds_train_lm <- lm(totrectds ~ log(tottargets) + teampasstds, train)

summary(rb_rec_tds_train_lm)

# ALL P-VALUES LESS THAN .05
# R-SQUARED .421
# ADJUSTED R-SQUARED .409

# USE MODEL TO CREATE PREDICTIONS ON TEST SET

test$predictionsrectds <- predict(rb_rec_tds_train_lm, test)

# DETERMINE RMSE AND MAE ON PREDICTIONS

rmse(test$totrectds, test$predictionsrectds)

mae(test$totrectds, test$predictionsrectds)

# RMSE 1.38

# MAE 1.17

# CREATE A MODEL TO PREDICT TOTAL RECEIVING YARDS ON TRAINING DATA

rb_rec_yards_train_lm <- lm(totrecyards ~ tottargets + longestreception, train)

summary(rb_rec_yards_train_lm)

# ALL P-VALUES LESS THAN .05
# R-SQUARED .911
# ADJUSTED R-SQUARED .909

# USE MODEL TO CREATE PREDICTIONS ON TEST SET

test$predictionsrecyards <- predict(rb_rec_yards_train_lm, test)

# DETERMINE RMSE AND MAE PREDICTIONS ON TEST SET

rmse(test$totrecyards, test$predictionsrecyards)

mae(test$totrecyards, test$predictionsrecyards)

# RMSE 44.32

# MAE 35.82
