

path_export <-
  file.path("data-raw", paste0("scores-", "2006-2017", "-espn.csv"))
if (export) {
  # data_debug <-
  #   expand.grid(season = 2007L,
  #               # seasontype = 1L:3L,
  #               # seasontype = 1L,
  #               seasontype = 3L,
  #               # wk = 1L:2L
  #               wk = 1L:5L) %>%
  #   as_tibble() %>%
  #   mutate(data = purrr::pmap(
  #     list(season, seasontype, wk),
  #     ~ do_get_scores_nfl_espn(
  #       season = ..1,
  #       seasontype = ..2,
  #       wk = ..3
  #     )
  #   ))
  
  grid <- tibble(season = seq(2006L, 2017L, 1L))
  # grid <- tibble(season = seq(2006L, 2007L, 1L))
  data <-
    grid %>%
    mutate(data = purrr::map(season, ~ do_get_scores_season_nfl_espn(season = .x)))
  # data %>%
  #   mutate(path_export = file.path("data-raw", paste0("scores-", season, "-espn.csv"))) %>%
  #   mutate(data = purrr::walk2(data, path_export, ~teproj::export_path(x = .x, path = .y))) %>%
  #   mutate(export = if_else(file.exists(path_export), TRUE, FALSE))
  
  
  data %>%
    select(-season) %>%
    unnest() %>%
    teproj::export_path(path = path_export)
} else {
  data <-
    teproj::import_path_cleanly(path_export, col_types = readr::cols())
  
}
