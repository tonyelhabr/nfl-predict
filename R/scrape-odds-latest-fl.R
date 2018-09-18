
fl_lines <-
  do_get_lines_latest_nfl_fl()

fl_lines
fl_lines %>% teproj::export_path(config$path_lines_temp)

