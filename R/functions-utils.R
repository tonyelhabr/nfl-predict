
get_current_season <-
  function(...) {
    config::get()$season_current
  }

# TODO!
# Need a `date` field. Then can do a `fuzzy_left_join()`.
get_current_wk <-
  function(data, ..., .season = config::get()$season_current) {
    nfl_game_result_trim <-
      import_nfl_game_result() %>% 
      filter(season == .season) %>% 
      select(season, wk, tm_home, tm_away)
    
    # ...
  }