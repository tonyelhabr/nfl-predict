
.season <- 2006L:2017L

data <- do_get_odds_nfl_pfref(season = .season)
data

data %>% teproj::export_path(config$path_odds_hist_nfl_pfref)
