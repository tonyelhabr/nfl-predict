
.get_cols_max_at <-
  function(data, col, rgx_split) {
    col_sym <- sym(col)
    data %>%
      pull(!!col_sym) %>% 
      str_split(rgx_split) %>% 
      map_dbl(~length(.)) %>% 
      max()
  }

.separate_cols_max_at <-
  function(data, col, n_cols_max = NULL, rgx_split, ...) {
    col_sym <- sym(col)
    if(is.null(n_cols_max)) {
      n_cols_max <-
      .get_cols_max_at(data = data, col = col, rgx_split = rgx_split)
    }
    nms_sep <-
      paste0(col, seq(1, n_cols_max, by = 1))
    data %>%
      separate(!!col_sym, into = nms_sep, sep = rgx_split, fill = "right")
  }

.convert_list_to_tbl <-
  function(x) {
    tibble::enframe(unlist(x))
  }

.recode_tm_cols_sport_strictly_at <-
  function(data, col, status = 1L, ..., .data_source) {
    .status <- status
    col_sym <- sym(col)
    tm_trim <-
      .data_source %>% 
      filter(status %in% .status) %>% 
      select(tm, tm_other = !!col_sym)
    data %>%
      inner_join(tm_trim, by = c("tm_home" = "tm_other")) %>% 
      mutate(tm_home = tm) %>% 
      select(-tm) %>% 
      inner_join(tm_trim, by = c("tm_away" = "tm_other")) %>% 
      mutate(tm_away = tm) %>% 
      select(-tm)
  }

# NOTE: Allow for "inactive" teams to also be considered via the status field.
.recode_tm_cols_nfl_strictly_at <-
  function(..., .data_source = import_nfl_tm()) {
    .recode_tm_cols_sport_strictly_at(..., .data_source = .data_source)
  }

.recode_tm_cols_sport_cautiously_at <-
  function(data, col, status = 0L:1L, ..., .data_source) {
    .status <- status
    col_sym <- sym(col)
    tm_trim <-
      .data_source %>% 
      filter(status %in% .status) %>% 
      select(tm, tm_other = !!col_sym)

    data %>%
      left_join(tm_trim, by = c("tm_home" = "tm_other")) %>% 
      mutate(tm_home = coalesce(tm, tm_home)) %>% 
      select(-tm) %>% 
      left_join(tm_trim, by = c("tm_away" = "tm_other")) %>% 
      mutate(tm_away = coalesce(tm, tm_away)) %>% 
      select(-tm)
  }

.recode_tm_cols_nfl_cautiously_at <-
  function(..., .data_source = import_nfl_tm()) {
    .recode_tm_cols_sport_cautiously_at(..., .data_source = .data_source)
  }

.add_wk_col_at <-
  function(data, val, col = "wk") {
    col_sym <- sym(col)
    data %>%
      mutate(!!col := as.integer(val))
  }

.add_season_col_at <-
  function(data, val, col = "season") {
    col_sym <- sym(col)
    data %>%
      mutate(!!col := as.integer(val))
  }

# NOTE: I think I want to NOT use this function. Instead,
# the timeperiod columns should be added given a `tibble` with `season` and `wk` columns.
.add_timeperiod_cols_nfl <-
  function(data, ..., season = config::get()$season_current) {
    if(season != config::get()$season_current) {
      stop("Not currently implemented.", call. = FALSE)
    }
    .season <- season
    nfl_game_result_trim <-
      import_nfl_game_result() %>% 
      filter(season == .season) %>% 
      select(season, wk, tm_home, tm_away)
    
    col_names_out <- c("season", "wk", names(data))
    data %>%
      left_join(
        nfl_game_result_trim,
        by = c("tm_home", "tm_away")
      ) %>% 
      select(one_of(col_names_out))
  }

# NOTE: This is a hard-coded value that doesn't need to be known by the user.
.SEASON_MIN_TONY <- 2012L
.arrange_gm_nfl <-
  function(data, ..., season = config::get()$season_current) {
    # NOTE: This is the more "correct" way of getting the minimum season value, 
    # but it results in the `nfl_game_result` data getting hit every time this function is used.
    # season_min <-
    #   import_nfl_game_result() %>% 
    #   filter(season == min(season)) %>% 
    #   distinct(season) %>% 
    #   pull(season)
    .season <- season
    if(.season < .SEASON_MIN_TONY) {
      return(data)
    }

    nfl_game_result_trim <-
      import_nfl_game_result() %>% 
      filter(season == .season) %>% 
      select(season, wk, tm_home, tm_away) %>% 
      mutate(rn = row_number())
    
    data %>%
      inner_join(nfl_game_result_trim) %>% 
      arrange(rn) %>% 
      select(-rn)
    
  }