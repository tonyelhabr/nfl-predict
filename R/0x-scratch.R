
data_raw <- get_odds_nba_tr()
data_raw %>% 
  .finalize_odds_nba_tr() %>% 
  # .fix_tm_cols_nba_tr_at() %>% 
  # .reorder_cols_nba_at() %>% 
  .add_scrape_cols_at()
