
data <- do_get_schedule_nba_bbref()
data %>% teproj::export_path(config$path_schedule_nba_bbref)
