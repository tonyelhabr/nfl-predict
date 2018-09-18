
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
  function(data, col, rgx_split, ..., warn = FALSE) {
    col_sym <- sym(col)
    n_cols_max <-
      .get_cols_max_at(data = data, col = col, rgx_split = rgx_split)
    nms_sep <-
      paste0(col, seq(1, n_cols_max, by = 1))
    .warn <- warn
    data %>%
      separate(!!col_sym, into = nms_sep, sep = rgx_split, warn = .warn)
  }

.convert_list_to_tbl <-
  function(x) {
    tibble::enframe(unlist(x))
  }


# .convert_list_to_tbl_.cleanly_at <-
#   function(x, col, rgx_split) {
#     data <- tibble::enframe(unlist(x))
#     .separate_cols_max_at(data = data, col = col, rgx_split = rgx_split)
#   }
# 
# .convert_list_to_tbl_.cleanly_espn_at <-
#   function(..., col = .COL_ESPN, rgx_split = .RGX_SPLIT_ESPN) {
#     .convert_list_to_tbl_.cleanly_at(..., col = col, rgx_split = rgx_split)
#   }


.recode_tm_cols_at <-
  function(data, col, ...) {
    
    col_sym <- sym(col)
    nfl_tm_trim <-
      import_nfl_tm() %>% 
      filter(status == 1L) %>% 
      select(tm, tm_other = !!col_sym)
    data %>%
      inner_join(nfl_tm_trim, by = c("tm_home" = "tm_other")) %>% 
      mutate(tm_home = tm) %>% 
      select(-tm) %>% 
      inner_join(nfl_tm_trim, by = c("tm_away" = "tm_other")) %>% 
      mutate(tm_away = tm) %>% 
      select(-tm)
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
.SEASON_MIN <- 2012L
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
    if(.season < .SEASON_MIN) {
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