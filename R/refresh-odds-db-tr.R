
conn <- get_db_conn_odds_tr()

odds_nfl_tr_exist <- import_odds_nfl_tr()
# odds_nba_tr_exist <- import_odds_nba_tr()

# conn <- get_db_conn_odds_tr()
# DBI::dbRemoveTable(conn = conn, name = "odds_nba_tr")
# odds_nba_tr_exist %>%
#   mutate(id_record = row_number()) %>% 
#   mutate(timestamp_record = timestamp_scrape) %>% 
#   insert_into_db(conn = conn, table = "odds_nba_tr", overwrite = TRUE)

# DBI::dbCreateTable(con = conn, "odds_tr", odds_tr)
DBI::dbListTables(conn = conn)
# DBI::dbRemoveTable(conn = conn, "odds_nba_tr")
# odds_tr %>% get_distinct_tms_at()

# odds_nfl_tr_distinct <-
#   odds_nfl_tr_exist %>%
#   distinctify_data_at()
# odds_nfl_tr_distinct

odds_nfl_tr_bad <-
  odds_nfl_tr_exist %>%
  filter(
    wk == 11L,
    tm_home == "LAR",
    tm_away == "KCY",
    # timestamp_scrape >= lubridate::ymd("2018-11-13")
    is.na(moneyline_home)
  )
odds_nfl_tr_bad
odds_nfl_tr_fix <-
  odds_nfl_tr_exist %>% 
  anti_join(odds_nfl_tr_bad)

# odds_nfl_tr_distinct %>% 
#   # .reorder_cols_nfl_at() %>% 
#   # mutate(id_record = row_number()) %>% 
#   insert_into_db(
#     conn = conn,
#     # table = "odds_tr",
#     overwrite = TRUE
#   )
odds_nfl_tr_fix %>% 
  .arrange_gm_nfl() %>% 
  insert_into_db(
    conn = conn,
    table = "odds_nfl_tr",
    overwrite = TRUE
  )
drop_db_conn(conn)
