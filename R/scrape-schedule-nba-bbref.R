
sched_bbref <-
  do_get_schedule_nba_bbref()

sched_bbref %>% teproj::export_path(config$path_schedule_nba_bbref)
