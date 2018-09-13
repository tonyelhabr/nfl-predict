
paths_funcs <-
  list.files(
    path = "R",
    pattern = "func",
    recursive = FALSE,
    full.names = TRUE
  )
invisible(sapply(paths_funcs, source))

conn <- get_db_conn()
nfl_game_odds_read <-
  read_from_db(
    conn = conn,
    table = "nfl_game_odds"
  ) %>% 
  convert_date_cols_at() %>% 
  convert_timestamp_cols_at()
nfl_game_odds_read

# nfl_game_odds %>%
#   get_distinct_tms_at()

# Preview...
nfl_game_odds_distinct <-
  nfl_game_odds_read %>%
  distinctify_data_at()
nfl_game_odds_distinct

nfl_game_odds_distinct %>% 
  # mutate(record_id = row_number()) %>% 
  insert_into_db(
    conn = conn,
    table = "nfl_game_odds",
    overwrite = TRUE
  )

conn %>% drop_db_conn()