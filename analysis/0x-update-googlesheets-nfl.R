

odds_nfl_tr_exist <- import_odds_nfl_tr()

.wk <- 1L
.wk_lag1 <- .wk - 1L

# scores_lag1 <- do_get_scores_nfl_espn(wk = .wk_lag1)
# scores_lag1_trim <-
#   scores_lag1 %>%
#   select(tm_home, tm_away, pts_home, pts_away)
# scores_lag1_trim
# teproj::export_path(
#   scores_lag1_trim,
#   config$path_scores_lag1_nfl_temp
# )
# if(interactive()) {
#   file.show(config$path_scores_lag1_nfl_temp)
# }
# FIXME!
odds_nfl_tr_lag1_close <-
  odds_nfl_tr_exist %>% 
  # drop_na() %>% 
  filter(wk == .wk_lag1) %>% 
  # filter(wk == .wk) %>% 
  # filter(date == max(date) | date == (max(date) - lubridate::days(1))) %>% 
  # filter(date == (max(date) - lubridate::days(7)) | date == (max(date) - lubridate::days(8))) %>% 
  filter(date == max(date)) %>% 
  mutate(gm = sprintf("%s v %s", tm_home, tm_away)) %>% 
  group_by(gm) %>% 
  filter(row_number(desc(timestamp_scrape)) == 1L) %>% 
  ungroup() %>% 
  # .arrange_gm_nfl()
  arrange(time)
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

odds_nfl_tr_wk <-
  odds_nfl_tr_exist %>%
  # drop_na() %>% 
  filter(wk == .wk) %>%
  # filter(wk == .wk_lag1) %>%
  # mutate(
  #   is_first = 
  #     case_when(
  #       date >= (max(date) - lubridate::days(7)) ~ TRUE,
  #       date == (max(date) - lubridate::days(8)) ~ TRUE,
  #       TRUE ~ FALSE
  #     )
  # ) %>% 
  # mutate(
  #   is_latest = 
  #     case_when(
  #       date == max(date) ~ TRUE,
  #       date == (max(date) - lubridate::days(1)) ~ TRUE,
  #       TRUE ~ FALSE
  #     )
  # ) %>% 
  mutate(gm = sprintf("%s v %s", tm_home, tm_away)) %>%
  filter(!is.na(spread_home) & !is.na(total)) %>%
  group_by(gm) %>%
  mutate(idx_intragm = row_number(timestamp_scrape)) %>% 
  ungroup() %>% 
  select(idx_intragm, gm, everything()) %>% 
  arrange(time)
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
        totalclose = total,
        timestamp_scrape_close = timestamp_scrape
      )
  ) %>%
  select(
    -matches('timestamp'),
    everything(),
    timestamp_scrape_open,
    timestamp_scrape_close
  ) %>%
  .arrange_gm_nfl()
odds_nfl_tr_wk_join

teproj::export_path(
  odds_nfl_tr_wk_join,
  config$path_odds_nfl_temp
)
if(interactive()) {
  file.show(config$path_odds_nfl_temp)
}


