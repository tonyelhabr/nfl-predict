
# rm(list = ls())
# suppressMessages(pacman::p_unload(pacman::p_loaded(), character.only = TRUE))

setwd("C:/Users/aelhabr/Documents/projects/sports-predict")
invisible(source(".Rprofile"))
# suppressPackageStartupMessages(library("tidyverse"))

odds_nfl_tr <- do_get_odds_nfl_tr()
success <- insert_into_db_odds_tr(data = odds_nfl_tr)
odds_nba_tr <- do_get_odds_nba_tr()
success <- insert_into_db_odds_tr(data = odds_nba_tr)
# success

# paths <-
#   list.files(
#     path = "data",
#     pattern = "sqlite$",
#     full.names = TRUE
#   )
# sort(paths)
# paths_keep <-
#   rev(sort(paths))[1:2]
# paths_rm <-
#   setdiff(paths, paths_keep)
# invisible(sapply(paths_rm, unlink))
