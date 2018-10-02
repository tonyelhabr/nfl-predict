
# rm(list = ls())
# suppressMessages(pacman::p_unload(pacman::p_loaded(), character.only = TRUE))

setwd("C:/Users/aelhabr/Documents/projects/nfl-predict")
invisible(source(".Rprofile"))
# suppressPackageStartupMessages(library("tidyverse"))

odds_tr <- do_get_odds_nfl_tr()
# odds_tr
# odds_tr %>% teproj::export_path(config$path_odds_temp)
success <- insert_into_db_odds_tr(data = odds_tr)
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
