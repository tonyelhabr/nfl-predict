
.download <- FALSE
odds <- do_get_odds_latest_nfl_fl(download = .download)
odds
odds %>% write_csv(config$path_odds_temp)

# debugging ----
path <-
  .get_grid_pre_nfl_fl() %>% 
  filter(wk == 5L) %>% 
  # .get_grid_post_nfl_fl() %>% 
  # filter(wk == 4L) %>% 
  mutate(path = ..get_path_nfl_fl(url)) %>% 
  pull(path)
path
data_raw <- path %>% .filter_odds_nfl_fl()
data_raw
data_raw_long <- data_raw %>% .clean_odds_raw_nfl_fl()
data_raw_long
data_raw_wide <- data_raw_long %>% .spread_odds_raw_nfl_fl()
data_clean <- data_raw_wide %>% .clean_odds_wide_nfl_fl()
data_clean

# debugging, cont. ----
odds_all <- do_get_odds_all_nfl_fl()
odds_all %>% filter(seasontype == 1L) %>% count(wk)
odds_all %>% filter(seasontype == 2L) %>% count(wk)
odds_all %>% filter(seasontype == 3L) %>% count(wk)
odds_1yr <- do_get_odds_season_nfl_fl(season = 2017L)
odds_1yr %>% filter(seasontype == 1L)
odds_1yr %>% filter(seasontype == 2L)
odds_1yr %>% filter(seasontype == 3L)
