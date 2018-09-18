
.COL_ESPN <- "name"
.RGX_SPLIT_ESPN <- "\\."
.get_cols_max_at <-
  function(data, col, rgx_split) {
    col_sym <- sym(col)
    data %>%
      pull(!!col_sym) %>% 
      str_split(rgx_split) %>% 
      map_dbl(~length(.)) %>% 
      max()
  }

.get_cols_max_espn_at <-
  function(..., col = .COL_ESPN, rgx_split = .RGX_SPLIT_ESPN) {
    .get_cols_max_at(..., col = col, rgx_split = rgx_split)
  }

.separate_cols_max_at <-
  function(data, col, rgx_split) {
    col_sym <- sym(col)
    n_cols_max <-
      .get_cols_max_at(data = data, col = col, rgx_split = rgx_split)
    nms_sep <-
      paste0(col, seq(1, n_cols_max, by = 1))
    data %>%
      separate(!!col_sym, into = nms_sep, sep = rgx_split)
  }

.separate_cols_max_espn_at <-
  function(..., col = .COL_ESPN, rgx_split = .RGX_SPLIT_ESPN) {
    .separate_cols_max_at(..., col = col, rgx_split = rgx_split)
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

.recode_tm_cols_espn <-
  function(data, ...) {

    .recode_tm_cols_at(
      data = data,
      col = "tm_espn"
    )
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

.add_timeperiod_cols_nfl <-
  function(data, ..., .season = config::get()$season_current) {
    if(.season != config::get()$season_current) {
      stop("Not currently implemented.", call. = FALSE)
    }
    nfl_game_result_trim <-
      import_nfl_game_result() %>% 
      filter(season == .season) %>% 
      select(season, wk, tm_home, tm_away)
    
    # browser()
    col_names_out <- c("season", "wk", names(data))
    data %>%
      left_join(
        nfl_game_result_trim,
        by = c("tm_home", "tm_away")
      ) %>% 
      select(one_of(col_names_out))
  }

.arrange_gm <-
  function(data, ..., .season = config::get()$season_current) {
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


.filter_scores_nfl_espn <-
  function(data, ...) {
    data %>%
      .separate_cols_max_espn_at(col = "name") %>% 
      filter((name1 == "events" &
                name2 == "shortName") |
               (
                 name1 == "events" &
                   name2 == "competitions" &
                   name3 == "competitors" &
                   name4 == "score"
               ))
  }

.clean_scores_nfl_espn <-
  function(data, ...) {
    data %>%
      select(isscore = name4, value) %>%
      mutate_at(vars(isscore), funs(if_else(is.na(.), FALSE, TRUE))) %>%
      mutate(gm = if_else(!isscore, value, NA_character_)) %>%
      fill(gm, .direction = "down") %>%
      # filter(!is.na(odds_type)) %>%
      filter(gm != value) %>%
      group_by(gm) %>%
      mutate(rn = row_number()) %>%
      ungroup() %>%
      mutate(tm_dir = if_else(rn == 1, "pts_home", "pts_away")) %>%
      select(gm, tm_dir, value) %>%
      spread(tm_dir, value) %>%
      separate(gm, into = c("tm_away", "tm_home"), sep = "(\\s+\\@\\s+)|(\\s+vs.*\\s+)") %>%
      mutate_at(vars(matches("pts")), funs(as.integer)) %>% 
      select(tm_home, tm_away, pts_home, pts_away)
  }

.finalize_scores_nfl_espn <-
  function(data, ...) {
    data %>%
      .recode_tm_cols_espn(...) %>%
      .add_timeperiod_cols_nfl(...) %>% 
      .arrange_gm(...)
  }


do_get_scores_nfl_espn <-
  function(wk, season = config::get()$season_current, ..., seasontype = 2L) {
    data_raw <-
      espn2::get_scores_nfl(week = wk, year = season, seasontype = seasontype, as = "minimal")
    data_raw %>%
      .filter_scores_nfl_espn(...) %>% 
      .clean_scores_nfl_espn(...) %>% 
      .finalize_scores_nfl_espn(...)
  }


.filter_odds_nfl_espn <-
  function(data, ...) {
    data %>% 
      .separate_cols_max_espn_at(col = "name") %>% 
      filter((name1 == "events" &
                name2 == "shortName") |
               (
                 name1 == "events" &
                   name2 == "competitions" &
                   name3 == "odds" &
                   name4 %in% c("details", "overUnder")
               ))
  }


.clean_odds_nfl_espn <-
  function(data, ...) {
    data %>%
      select(isodds = name3, odds_type = name4, value) %>%
      mutate_at(vars(isodds), funs(if_else(is.na(.), FALSE, TRUE))) %>%
      mutate(gm = if_else(!isodds, value, NA_character_)) %>%
      fill(gm, .direction = "down") %>%
      # filter(!is.na(odds_type)) %>%
      filter(gm != value) %>%
      select(gm, odds_type, value) %>%
      spread(odds_type, value) %>%
      separate(gm, into = c("tm_away", "tm_home"), sep = "(\\s+\\@\\s+)|(\\s+vs.*\\s+)") %>%
      separate(details, into = c("tm_favored", "spread"), sep = "\\s+") %>%
      mutate_at(vars(spread, overUnder), funs(as.numeric)) %>%
      mutate(spread_home = if_else(tm_home == tm_favored, spread, -spread)) %>%
      select(-tm_favored, -spread) %>% 
      rename(total = overUnder) %>%
      mutate_at(vars(total), funs(if_else(. <= 0, NA_real_, .))) 
  }

.finalize_odds_nfl_espn <-
  function(data, ...) {
    
    data %>%
      .recode_tm_cols_espn(...) %>%
      .add_timeperiod_cols_nfl_espn(...) %>% 
      .arrange_gm(...)
  }

do_get_odds_nfl_espn <-
  function(wk, season = config::get()$season_current, ..., seasontype = 2L) {
    data_raw <-
      espn2::get_scores_nfl(week = wk, year = season, seasontype = seasontype, as = "minimal")
    data_raw %>%
      .filter_odds_nfl_espn() %>% 
      .clean_odds_nfl_espn() %>% 
      .finalize_odds_nfl_espn()
  }

