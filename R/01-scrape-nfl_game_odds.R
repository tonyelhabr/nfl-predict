
# rm(list = ls())
# suppressMessages(pacman::p_unload(pacman::p_loaded(), character.only = TRUE))

suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("rlang"))

setwd("C:/Users/aelhabr/Documents/projects/nfl-predict")
paths_funcs <-
  list.files(
    path = "R",
    pattern = "func",
    recursive = FALSE,
    full.names = TRUE
  )
invisible(sapply(paths_funcs, source))

nfl_odds_req <-
  request_odds_nfl()
# nfl_odds_req

nfl_game_odds <-
  nfl_odds_req %>% 
  extract_nfl_game_odds() %>%
  add_scrape_cols_at()
# nfl_game_odds

conn <- get_db_conn()
# conn %>% DBI::dbListTables()

success <-
  insert_into_db(
    data = nfl_game_odds,
    conn = conn
  )
# success

conn %>% drop_db_conn()


