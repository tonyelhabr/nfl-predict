
get_season_current <-
  function(...) {
    config$season_current_nfl
  }

# TODO!
# Need a `date` field. Then can do a `fuzzy_left_join()`.
get_current_wk <-
  function(data, ..., season = config$season_current_nfl) {
    .season <- season
    nfl_game_result_trim <-
      import_nfl_game_result() %>% 
      filter(season == .season) %>% 
      select(season, wk, tm_home, tm_away)
    
    # ...
  }

.COLS_NFL_ORDER <-
  c(
    "season",
    "seasontype",
    "wk",
    "date",
    "time",
    "weekday",
    "location",
    "tm",
    "tm_home",
    "tm_away",
    "spread_home",
    "total",
    "moneyline_home",
    "moneyline_away"
  )
.COLS_NBA_ORDER <-
  c(
    "season",
    "seasontype",
    # "wk",
    "date",
    "time",
    "weekday",
    "location",
    "tm", 
    "tm_home",
    "tm_away",
    "spread_home",
    "total",
    "moneyline_home",
    "moneyline_away"
  )


.reorder_cols_sport_at <-
  function(data, ..., col_names_order) {
    col_names <- names(data)
    col_names_in <- intersect(col_names_order, col_names)
    col_names_nin <- setdiff(col_names, col_names_order)
    # length(col_names); length(col_names_in); length(col_names_nin); length(col_names_order)
    # setdiff(col_names, c(col_names_in, col_names_nin))
    col_names_fct <- factor(col_names, levels = c(col_names_in, col_names_nin))
    data %>% select(one_of(levels(col_names_fct)))
  }

.reorder_cols_nfl_at <-
  function(..., col_names_order = .COLS_NFL_ORDER) {
    .reorder_cols_sport_at(..., col_names_order = col_names_order)
  }

.reorder_cols_nba_at <-
  function(..., col_names_order = .COLS_NBA_ORDER) {
    .reorder_cols_sport_at(..., col_names_order = col_names_order)
  }


.select_cols_sport_at <-
  function(data, ..., col_names, col_names_order = col_names) {
    col_names_fct <- factor(col_names, levels = col_names_order)
    
    data %>% select(one_of(levels(col_names_fct)))
  }

.select_cols_nfl_at <-
  function(..., col_names = .COLS_NFL_ORDER) {
    .select_cols_sport_at(..., col_names = col_names)
  }

.select_cols_nba_at <-
  function(..., col_names = .COLS_NBA_ORDER) {
    .select_cols_sport_at(..., col_names = col_names)
  }

