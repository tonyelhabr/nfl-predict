
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
  function(data, col, ..., .status = 1L, .data_source) {
    # browser()
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
  function(data, col, ..., .status = 0L:1L, .data_source) {
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


.recode_tm_cols_nba_strictly_at <-
  function(..., .data_source = import_nba_tm()) {
    .recode_tm_cols_sport_strictly_at(..., .data_source = .data_source)
  }

.recode_tm_cols_nfl_cautiously_at <-
  function(..., .data_source = import_nfl_tm()) {
    .recode_tm_cols_sport_cautiously_at(..., .data_source = .data_source)
  }

.recode_tm_cols_nba_cautiously_at <-
  function(..., .data_source = import_nba_tm()) {
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

# TODO: UPdate these functions to use .data_source!
# NOTE: I think I want to NOT use this function. Instead,
# the timeperiod columns should be added given a `tibble` with `season` and `wk` columns.
.add_timeperiod_cols_nfl <-
  function(data, ..., .season = config::get()$season_current_nfl, .data_source = import_nfl_game_result()) {
    if(.season != config::get()$season_current_nfl) {
      stop("Not currently implemented.", call. = FALSE)
    }
    
    game_result_trim <-
      .data_source %>% 
      filter(season == .season) %>% 
      select(season, wk, tm_home, tm_away)
    
    col_names_out <- c("season", "wk", names(data))
    data %>%
      left_join(
        game_result_trim,
        by = c("tm_home", "tm_away")
      ) %>% 
      select(one_of(col_names_out))
  }

# NOTE: This is a hard-coded value that doesn't need to be known by the user.
.SEASON_MIN_TONY <- 2012L
.arrange_gm_nfl <-
  function(data, ..., .season = config::get()$season_current_nfl, .data_source = import_nfl_game_result()) {
    # NOTE: Not sure what is the most "correct" way of getting the minimum season value.

    if("season" %in% names(data)) {
      .seasons <-
        data %>% 
        distinct(season) %>% 
        pull(season)
      
      if(length(.seasons) > 1L) {
        stop("Expecting only one distinct season.", call. = FALSE)
        return(data)
      }
      
      .season <- .seasons
      if(.season < .SEASON_MIN_TONY) {
        return(data)
      }
    }

    game_result_trim <-
      .data_source %>% 
      filter(season == .season) %>% 
      select(season, wk, tm_home, tm_away) %>% 
      mutate(rn = row_number())
    
    data %>%
      inner_join(game_result_trim) %>% 
      arrange(rn) %>% 
      select(-rn)
    
  }