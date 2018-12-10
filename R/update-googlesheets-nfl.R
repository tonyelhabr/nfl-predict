

odds_nfl_tr_exist <- import_odds_nfl_tr()

.wk <- 15L
.wk_lag1 <- .wk - 1L

# undebug(do_get_scores_nfl_espn)
scores_lag1 <- do_get_scores_nfl_espn(wk = .wk_lag1)
scores_lag1_trim <-
  scores_lag1 %>%
  select(tm_home, tm_away, pts_home, pts_away)
scores_lag1_trim
teproj::export_path(
  scores_lag1_trim,
  config$path_scores_lag1_nfl_temp
)
if(interactive()) {
  file.show(config$path_scores_lag1_nfl_temp)
}

odds_nfl_tr_lag1_close <-
  odds_nfl_tr_exist %>% 
  filter(wk == .wk_lag1) %>% 
  mutate(gm = sprintf("%sv%s", tm_home, tm_away)) %>% 
  group_by(gm) %>% 
  filter(row_number(desc(timestamp_scrape)) == 1L) %>% 
  ungroup() %>% 
  .arrange_gm_nfl()
odds_nfl_tr_lag1_close
odds_nfl_tr_lag1_close_trim <-
  odds_nfl_tr_lag1_close %>% 
  select(tm_home, tm_away, spread_home, total, timestamp_scrape)
teproj::export_path(
  odds_nfl_tr_lag1_close_trim,
  config$path_odds_lag1_nfl_temp
)
if(interactive()) {
  file.show(config$path_odds_lag1_nfl_temp)
}

odds_nfl_tr_open <-
  odds_nfl_tr_exist %>%
  filter(wk == .wk) %>%
  mutate(gm = sprintf("%sv%s", tm_home, tm_away)) %>%
  filter(!is.na(spread_home) & !is.na(total)) %>%
  group_by(gm) %>%
  filter(row_number(timestamp_scrape) == 1L) %>%
  ungroup() %>%
  .arrange_gm_nfl()
odds_nfl_tr_open

odds_nfl_tr_open_trim <-
  odds_nfl_tr_open %>% 
  select(tm_home, tm_away, spread_home, total, timestamp_scrape)
teproj::export_path(
  odds_nfl_tr_open_trim,
  config$path_odds_nfl_temp
)
if(interactive()) {
  file.show(config$path_odds_nfl_temp)
}


