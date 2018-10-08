
.N_GM_PER_REG_PFREF <- 256L
.N_GM_PER_POST_PFREF <- 11L
.N_GM_PER_SEASON_PFREF <- .N_GM_PER_REG_PFREF + .N_GM_PER_POST_PFREF
.N_ROW_PER_PG_PFREF <- 100L
.N_PG_PER_SEASON_PFREF <- ceiling(.N_GM_PER_SEASON_PFREF / .N_ROW_PER_PG_PFREF)
.URL_ODDS_FORMAT_PFREF <-
  paste0(
    "https://www.pro-football-reference.com/play-index/tgl_finder.cgi?request=1&match=game&year_min=%.0f&year_max=%.0f",
    "&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&temperature_gtlt=lt&c1stat=vegas_line&c1comp=gt&c5val=1.0",
    "&order_by=game_date&order_by_asc=Y&offset=%.0f"
  )

# grid ----
.get_grid_url_odds_nfl_pfref <-
  function(season = as.integer(format(Sys.Date(), "%Y")), ...) {
    
    tibble(season = rep(season, .N_PG_PER_SEASON_PFREF)) %>% 
      # season = rep(2017L, 1)) %>%
      group_by(season) %>%
      mutate(n_pg = row_number()) %>%
      ungroup() %>%
      arrange(season, n_pg) %>% 
      mutate(url = sprintf(
        .URL_ODDS_FORMAT_PFREF,
        season,
        season,
        (n_pg - 1) * .N_ROW_PER_PG_PFREF
      )) %>% 
      select(-n_pg)
  }

# get ----
.get_odds_nfl_pfref <-
  function(url, ..., col_names = NULL) {
    # browser()
    html <-
      url %>%
      xml2::read_html()
    html_table <-
      html %>% 
      rvest::html_nodes("table") %>% 
      pluck(1)
    html_data <-
      html_table %>% 
      rvest::html_table(header = FALSE) 
    header <-
      html_data %>% 
      slice(2)
    if(is.null(col_names)) {
      col_names <- header %>% as.character()
    }
    
    html_data %>% 
      slice(3:n()) %>% 
      tibble::as_tibble() %>% 
      filter(X1 != col_names[1]) %>% 
      purrr::set_names(col_names)
  }




# clean ----
.clean_odds_nfl_pfref <-
  function(data, ...) {
    data %>%
      janitor::clean_names() %>%
      select(season = year, wk = week, everything()) %>% 
      mutate_at(
        vars(date),
        funs(lubridate::ymd(.))
      ) %>% 
      mutate_at(
        vars(time),
        funs(paste0(date, " ", .) %>% lubridate::ymd_hm())
      ) %>% 
      mutate(
        weekday = date %>% lubridate::wday(label = TRUE, abbr = TRUE)
      ) %>% 
      separate(result, into = c("wl", "score"), sep = "\\s") %>%
      separate(score, into = c("pts_tm", "pts_opp"), sep = "\\-") %>%
      mutate(
        ot = if_else(ot != "", TRUE, FALSE),
        tm_home = if_else(x == "", tm, opp),
        tm_away = if_else(x == "", opp, tm),
        pts_home = if_else(x == "", pts_tm, pts_opp),
        pts_away = if_else(x == "", pts_opp, pts_tm),
        total = over_under,
        total_result = 
          case_when(
            ou_result == "over" ~ "O",
            ou_result == "under" ~ "U",
            TRUE ~ "E"
          )
      ) %>% 
      mutate_at(vars(matches("season|wk|pts")), funs(as.integer)) %>% 
      mutate_at(vars(matches("spread|total$")), funs(as.double)) %>% 
      mutate(spread_home = if_else(x == "", spread, -spread)) %>% 
      mutate(
        tm_winner_straight = 
          case_when(
            pts_home > pts_away ~ tm_home,
            pts_home < pts_away ~ tm_away,
            TRUE ~ NA_character_
          ),
        tm_winner_spread = 
          case_when(
            (pts_home + spread_home) > pts_away ~ tm_home,
            (pts_home + spread_home) < pts_away ~ tm_away,
            TRUE ~ NA_character_
          )
      ) %>% 
      select(
        season,
        wk,
        date,
        time,
        weekday,
        tm_home,
        tm_away,
        pts_home,
        pts_away,
        ot,
        tm_winner_straight,
        spread_home,
        tm_winner_spread,
        total,
        total_result
      )
  }

# postprocess ----
.recode_tm_cols_nfl_pfref <-
  function(data, col = "tm", ...) {
    # .recode_tm_cols_nfl_strictly_at(data = data, col = col, ...)
    data
  }

# do ----
do_get_odds_nfl_pfref <-
  function(season = config::get()$season_current,
           ...,
           # wk = 1L:17L,
           # seasontype = 1L:3L,
           .arrange = ifelse(season == config::get()$season_current, TRUE, FALSE)) {
    
    # .seasontype <- seasontype
    # .wk <- wk
    grid <-
      .get_grid_url_odds_nfl_pfref(season = season, ...)
    # res <-
    #   res %>%
    #   filter(seasontype %in% .seasontype) %>%
    #   filter(wk %in% .wk)
    res <-
      grid %>%
      mutate(
        data = purrr::map(url, ~ .get_odds_nfl_pfref(data = .x, ...))
      )
    res <-
      res %>%
      mutate(
        data = purrr::map(data, ~ .clean_odds_nfl_pfref(data = .x, ...))
      )
    # NOTE: I don't think this is necessary, except for maybe seasons where 
    # team names are different than the current ones.
    # res <-
    #   res %>% 
    #   .recode_tm_cols_nfl_pfref(...)
    if (.arrange) {
      res <-
        res %>% 
        .arrange_gm_nfl(..., .season = .season)
    }
    
    res <-
      res %>% 
      select(-season, -url) %>% 
      unnest() %>%
      .reorder_cols_nfl_at(...)
    res
    
  }
