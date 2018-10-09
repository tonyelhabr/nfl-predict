

.URL_SCHEDULE_NBA_BBREF_FMT <-
  "https://www.basketball-reference.com/leagues/NBA_%s_games-%s.html"

.get_grid_url_schedule_nba_bbref <-
  function(season = config::get()$season_current_nba, ...) {
    grid <-
      expand.grid(
        season = season + 1L,
        month = c(10L:12L, 1L:4L)) %>%
      as_tibble() %>%
      mutate(
        month = month %>% 
          lubridate::month(label = TRUE, abbr = FALSE) %>% 
          tolower()
      ) %>%
      mutate(url = sprintf(.URL_SCHEDULE_NBA_BBREF_FMT, season, month))
  }

.COLS_SCHEDULE_NBA_BBREF <-
  c(
    "date",
    "time",
    "tm_away",
    "pts_away",
    "tm_home",
    "pts_home",
    "box_score_link",
    "ot",
    "attendance",
    "notes"
  )
.get_schedule_nba_bbref <-
  function(url, ..., col_names = .COLS_SCHEDULE_NBA_BBREF) {
    html <-
      url %>% 
      xml2::read_html()
    
    html_table <-
      html %>% 
      rvest::html_table(header = FALSE) %>% 
      pluck(1) %>% 
      as_tibble() %>% 
      slice(2:n())
    if(is.null(col_names)) {
      col_names <-
        html %>% 
        rvest::html_table(header = FALSE) %>% 
        pluck(1) %>% 
        as_tibble() %>% 
        slice(1) %>% 
        as.character()
    }
    html_table %>%
      set_names(col_names) %>% 
      select(date, time, tm_home, tm_away, pts_home, pts_away, ot)
  }

.clean_schedule_nba_bbref <-
  function(data, ...) {
    data %>% 
      mutate_at(
        vars(date),
        funs(strptime(., "%a, %b %d, %Y") %>% lubridate::ymd())
      ) %>% 
      mutate_at(
        vars(time),
        funs(paste0(date, " ", .) %>% lubridate::ymd_hm())
      ) %>% 
      mutate(
        weekday = date %>% lubridate::wday(label = TRUE, abbr = TRUE)
      ) %>% 
      mutate_at(vars(matches("^pts_")), funs(as.integer))
  }

.recode_tm_cols_nba_bbref <-
  function(data, col = "tm_name_full", ...) {
    .recode_tm_cols_nba_cautiously_at(data = data, col = col, ...)
  }

do_get_schedule_nba_bbref <-
  function(season = config::get()$season_current_nba,
           ...,
           .arrange = FALSE) {
    grid <-
      .get_grid_url_schedule_nba_bbref(season = season, ...)
    
    .get_schedule_nba_bbref_safe <-
      purrr::possibly(.get_schedule_nba_bbref, otherwise = tibble())
    res <-
      grid %>%
      mutate(
        data = purrr::map(url, ~ .get_schedule_nba_bbref_safe(data = .x, ...))
      ) %>% 
      # mutate(
      #   data = purrr::map(data, ~ .clean_schedule_nba_bbref(data = .x, ...))
      # ) %>% 
      select(-month, -url) %>% 
      unnest() %>% 
      .clean_schedule_nba_bbref(...) %>% 
      .recode_tm_cols_nba_bbref(...) %>% 
      .reorder_cols_nba_at(...)
    # if (.arrange) {
    #   res <-
    #     res %>% 
    #     .arrange_gm_nba(..., season = season)
    # }
    res
  }
    
