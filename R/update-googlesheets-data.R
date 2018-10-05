
.wk <- 6L
.wk_lag1 <- .wk - 1L

scores_lag1 <- do_get_scores_nfl_espn(wk = .wk_lag1)
scores_lag1_trim <-
  scores_lag1 %>%
  select(tm_home, tm_away, pts_home, pts_away)
scores_lag1_trim
scores_lag1_trim %>% teproj::export_path(config$path_scores_lag1_temp)

odds_tr_exist <- import_odds_tr()

odds_tr_lag1_close <-
  odds_tr_exist %>% 
  filter(wk == .wk_lag1) %>% 
  mutate(gm = sprintf("%sv%s", tm_home, tm_away)) %>% 
  group_by(gm) %>% 
  filter(row_number(desc(timestamp_scrape)) == 1L) %>% 
  ungroup() %>% 
  .arrange_gm_nfl()
odds_tr_lag1_close
odds_tr_lag1_close_trim <-
  select(tm_home, tm_away, spread_home, total, timestamp_scrape)
odds_tr_lag1_close_trim %>% teproj::export_path(config$path_odds_lag1_temp)

odds_tr_open <-
  odds_tr_exist %>%
  filter(wk == .wk) %>%
  mutate(gm = sprintf("%sv%s", tm_home, tm_away)) %>%
  filter(!is.na(spread_home) & !is.na(total)) %>%
  group_by(gm) %>%
  filter(row_number(timestamp_scrape) == 1L) %>%
  ungroup() %>%
  # right_join(
  #   odds_tr_exist %>% 
  #     filter(wk == .wk) %>% 
  #     select(season, wk, tm_home, tm_away)
  # ) %>%
  # .reorder_cols_nfl_at() %>%
  .arrange_gm_nfl()
odds_tr_open
odds_tr_open_trim <-
  odds_tr_open %>% 
  select(tm_home, tm_away, spread_home, total, timestamp_scrape)
odds_tr_open_trim %>% teproj::export_path(config$path_odds_temp)

# Or...
.download <- FALSE
odds_fl_open <- do_get_odds_latest_nfl_fl(download = .download)
odds_fl_open

