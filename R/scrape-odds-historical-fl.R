
# new ----
path <-
  # .get_grid_pre_nfl_fl() %>% 
  # filter(wk == 1L) %>% 
  .get_grid_post_nfl_fl() %>% 
  filter(wk == 4L) %>% 
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

data_all <- do_get_odds_all_nfl_fl()
data_all %>% filter(seasontype == 1L) %>% count(wk)
data_all %>% filter(seasontype == 2L) %>% count(wk)
data_all %>% filter(seasontype == 3L) %>% count(wk)
data <- do_get_odds_season_nfl_fl(season = 2017L)
data %>% filter(seasontype == 1L)
data %>% filter(seasontype == 2L)
data %>% filter(seasontype == 3L)
# Or...
data <- do_get_odds_nfl_fl(season = 2006L, wk = 1L:3L, seasontype = 1L:3L)


grid <- .preprocess_do_get_xxx_nfl_fl(season = 2016L, wk = 1L:17L, seasontype = 1L:3L)
data_raw1 <-
  grid %>%
  mutate(data = purrr::map(path, ~ .filter_odds_nfl_fl(path = .x)))
.seasontype <- 3L
.wk <- 4L
data_raw1 %>% 
  filter(seasontype == .seasontype) %>% 
  filter(wk == .wk) %>% 
  unnest(data) -> z1

# str_replace("&nbsp;", "\\&nbsp;", " ")
data_raw2 <-
  data_raw1 %>% 
  filter(seasontype == .seasontype) %>% 
  filter(wk == .wk) %>% 
  # mutate(data = purrr::map(data, ~ .do_clean_odds_nfl_fl(data = .x)))
  mutate(data = purrr::map(data, ~ .clean_odds_raw_nfl_fl(data = .x)))
data_raw2 %>% unnest(data) -> z2a
z2a
data_raw3 <-
  data_raw2 %>% 
  mutate(data = purrr::map(data, ~ .spread_odds_raw_nfl_fl(data = .x))) %>% 
  mutate(data = purrr::map(data, ~ .clean_odds_wide_nfl_fl(data = .x)))
data_raw2 %>% unnest(data) -> z2
data_raw2 %>% unnest(data)

# # old ----
.season <- 2017
grid <-
  .get_grid_nfl_fl() %>% 
  # mutate(season = .season) %>% 
  select(url) %>% 
  mutate(path = purrr::map_chr(url, ~..get_path_nfl_fl(.x)))
grid

# url <- "http://www.footballlocks.com/nfl_lines.shtml"
# path_dl <- "data-raw/nfl_lines.html"
grid1 <- grid %>% slice(6)
# url <- grid1 %>% pull(url)
path <- grid1 %>% pull(path)

fl <-
  path %>%
  .filter_odds_nfl_fl() %>% 
  .clean_odds_nfl_fl()
fl %>% .recode_tm_cols_fl() %>% select(season, wk, date, time, spread_home, total) %>% anti_join(fl)
fl %>% filter(season == 2016)
fl %>% .recode_tm_cols_fl() %>% filter(season == 2016) # count(season)
fl %>% count(season)


# fl_odds %>% write_csv(config$path_lines_temp)
