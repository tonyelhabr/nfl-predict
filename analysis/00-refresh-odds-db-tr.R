
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

odds_nfl_tr_exist %>% arrange(desc(timestamp_scrape))
odds_nfl_tr_exist %>%
  filter(is.na(season)) %>% 
  count(time, sort = TRUE)
odds_nfl_tr_distinct <-
  odds_nfl_tr_exist %>%
  distinctify_data_at()
odds_nfl_tr_distinct
odds_nfl_tr_distinct %>% arrange(desc(timestamp_scrape))

.wk17_date <-
  odds_nfl_tr_exist %>% 
  # filter(!is.na(wk)) %>% 
  filter(wk == 17L) %>% 
  filter(date == max(date)) %>% 
  slice(1) %>% 
  pull(date)
odds_nfl_tr_fix <-
  odds_nfl_tr_distinct %>% 
  mutate(season = 2018L) %>% 
  mutate_at(vars(wk), funs(as.integer)) %>% 
  mutate_at(
    vars(wk), 
    funs(
      if_else(date > .wk17_date,
              case_when(
                date <= .wk17_date + lubridate::days(1 + 1 * 7) ~ 19L,
                date <= .wk17_date + lubridate::days(1 + 2 * 7) ~ 19L,
                date <= .wk17_date + lubridate::days(1 + 3 * 7) ~ 20L,
                TRUE ~ 22L
              ),
              .
      )
    )
  ) 
odds_nfl_tr_fix

# odds_nfl_tr_bad <-
#   odds_nfl_tr_exist %>%
#   filter(
#     wk == 11L,
#     tm_home == "LAR",
#     tm_away == "KCY",
#     # timestamp_scrape >= lubridate::ymd("2018-11-13")
#     is.na(moneyline_home)
#   )
# odds_nfl_tr_bad
# odds_nfl_tr_fix <-
#   odds_nfl_tr_exist %>% 
#   anti_join(odds_nfl_tr_bad)

# odds_nfl_tr_distinct %>% 
#   # .reorder_cols_nfl_at() %>% 
#   # mutate(id_record = row_number()) %>% 
#   insert_into_db(
#     conn = conn,
#     # table = "odds_tr",
#     overwrite = TRUE
#   )
odds_nfl_tr_fix %>% 
  # .arrange_gm_nfl() %>% 
  arrange(season, wk, date, time, timestamp_scrape, id_record) %>% 
  insert_into_db(
    conn = conn,
    table = "odds_nfl_tr",
    overwrite = TRUE
  )
drop_db_conn(conn)
