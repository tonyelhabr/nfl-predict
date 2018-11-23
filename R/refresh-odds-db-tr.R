
conn <- get_db_conn_odds_tr()

odds_nfl_tr_exist <- import_odds_nfl_tr()
odds_nba_tr_exist <- import_odds_nba_tr()

conn <- get_db_conn_odds_tr()
# DBI::dbRemoveTable(conn = conn, name = "odds_nba_tr")
odds_nba_tr_exist %>%
  mutate(id_record = row_number()) %>% 
  mutate(timestamp_record = timestamp_scrape) %>% 
  insert_into_db(conn = conn, table = "odds_nba_tr", overwrite = TRUE)

# DBI::dbCreateTable(con = conn, "odds_tr", odds_tr)
DBI::dbListTables(conn = conn)
# DBI::dbRemoveTable(conn = conn, "odds_nba_tr")
# odds_tr %>% get_distinct_tms_at()

odds_nfl_tr_distinct <-
  odds_nfl_tr_exist %>%
  # mutate_at(vars(timestamp_scrape, timestamp_record), funs(as.POSIXct)) %>% 
  distinctify_data_at()
odds_tr_distinct

# odds_tr_distinct %>% 
#   # .reorder_cols_nfl_at() %>% 
#   # mutate(id_record = row_number()) %>% 
#   insert_into_db(
#     conn = conn,
#     # table = "odds_tr",
#     overwrite = TRUE
#   )
# insert_into_db(
#   odds_nfl_tr,
#   conn = conn,
#   overwrite = TRUE
# )
drop_db_conn(conn)
