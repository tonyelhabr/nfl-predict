
# rm(list = ls())
# suppressMessages(pacman::p_unload(pacman::p_loaded(), character.only = TRUE))

setwd("C:/Users/aelhabr/Documents/projects/nfl-predict")
invisible(source(".Rprofile"))
# suppressPackageStartupMessages(library("tidyverse"))

odds_tr <- do_get_odds_nfl_tr()
# odds_tr

success <- insert_into_db_odds_tr(data = odds_tr)
# success



