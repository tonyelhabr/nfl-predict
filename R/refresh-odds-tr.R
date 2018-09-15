
conn <- get_db_conn_odds_tr()

odds_tr_exist <-
  import_odds_tr()
odds_tr_exist

# DBI::dbCreateTable(con = conn, "odds_tr", odds_tr)
DBI::dbListTables(conn = conn)
# DBI::dbRemoveTable(conn = conn, "nfl_game_odds")
# odds_tr %>% get_distinct_tms_at()

odds_tr_distinct <-
  odds_tr_exist %>%
  rename(total = total_home) %>% 
  # mutate_at(vars(timestamp_scrape, timestamp_record), funs(as.POSIXct)) %>% 
  distinctify_data_at()
odds_tr_distinct

odds_tr_distinct %>% 
  # mutate(id_record = row_number()) %>% 
  insert_into_db(
    conn = conn,
    table = "odds_tr",
    overwrite = TRUE
  )
drop_db_conn(conn)
