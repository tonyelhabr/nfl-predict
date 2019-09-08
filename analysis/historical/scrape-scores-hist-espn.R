
.season <- 2006L:2017L
grid <- tibble(season = .season)

data <-
  grid %>%
  mutate(data = purrr::map(season, ~ do_get_scores_season_nfl_espn(season = .x))) %>% 
  select(-season) %>%
  unnest() 

data %>% teproj::export_path(config$path_scores_hist_nfl_espn)
