
today <- lubridate::today()
today_wday <- today %>% lubridate::wday(label = TRUE)
scrape_scores <- any(today_wday == c('Mon', 'Tue'))
scrape_scores
is_lag1 <- any(today_wday == c('Mon'))
is_lag1

odds_nfl_tr_exist <- import_odds_nfl_tr()

.wk <- 11L
.wk_lag1 <- .wk - 1L

if(scrape_scores) {
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
}

# ----
odds_nfl_tr_aug <-
  odds_nfl_tr_exist %>%
  # drop_na() %>% 
  # filter(wk == .wk) %>%
  mutate(gm = sprintf("%s vs %s", tm_home, tm_away)) %>%
  filter(!is.na(spread_home) & !is.na(total)) %>%
  group_by(season, wk, gm) %>%
  mutate(idx_intragm = row_number(timestamp_scrape)) %>% 
  ungroup() %>% 
  select(idx_intragm, gm, everything()) %>% 
  arrange(time)
odds_nfl_tr_aug

odds_nfl_tr_wk <-
  odds_nfl_tr_aug %>% 
  filter(season == config$season_current_nfl & wk == .wk)
odds_nfl_tr_wk

odds_nfl_tr_wk_first <-
  odds_nfl_tr_wk %>% 
  filter(idx_intragm == 1L) %>%
  ungroup() %>%
  arrange(time)
odds_nfl_tr_wk_first

odds_nfl_tr_wk_latest <-
  odds_nfl_tr_wk %>%
  group_by(gm) %>% 
  filter(idx_intragm == max(idx_intragm)) %>%
  ungroup() %>%
  arrange(time)
odds_nfl_tr_wk_latest

odds_nfl_tr_wk_join <-
  full_join(
    odds_nfl_tr_wk_first %>%
      select(
        tm_home,
        tm_away,
        spread_home_open = spread_home,
        total_open = total,
        timestamp_scrape_open = timestamp_scrape
      ),
    odds_nfl_tr_wk_latest %>%
      select(
        tm_home,
        tm_away,
        spread_home_close = spread_home,
        total_close = total,
        timestamp_scrape_close = timestamp_scrape
      )
  ) %>%
  .arrange_gm_nfl() %>% 
  select(
    season,
    wk,
    tm_home,
    tm_away,
    spread_home_open,
    total_open,
    spread_home_close,
    total_close,
    timestamp_scrape_open,
    timestamp_scrape_close
  )
odds_nfl_tr_wk_join

# path <- ifelse(is_lag1, config$path_odds_lag1_nfl_temp, config$path_odds_nfl_temp)
path <- config$path_odds_nfl_temp
teproj::export_path(
  odds_nfl_tr_wk_join,
  path
)
if(interactive()) {
  file.show(path)
}

