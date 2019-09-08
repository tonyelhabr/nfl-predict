
.URL_WINTOTALS_NFL_SPORTSODDSHISTORY <-
  "http://www.sportsoddshistory.com/nfl-regular-season-win-total-results-by-team/"
# .URL_WINTOTALS_NBA_SPORTSODDSHISTORY_FMT <- "http://www.sportsoddshistory.com/nba-win/?y=2017-2018&sa=nba&t=win&o=t"

.COLS_WINTOTALS_NFL_SPORTSODDSHISTORY <-
  c("tm_name_full",
    paste0("season_", seq(2010L, 2018L, 1L)),
    paste0("ou_", seq(1990L, 2010L, 10L)),
    "ou_all",
    "streak")

.get_wintotals_nfl_sportsoddshistory <-
  function(...,
           path = NULL,
           url = .URL_WINTOTALS_NFL_SPORTSODDSHISTORY,
           col_names = .COLS_WINTOTALS_NFL_SPORTSODDSHISTORY) {
    if (!is.null(path)) {
      if (!file.exists(path)) {
        msg <-
          sprintf(
            "`path` (%s) does not exist. Trying to retrieve data from `url` (%s) instead.",
            path,
            url
          )
        warning(msg, call. = FALSE)
      } else {
        url <- path
      }
      
    }
    html <-
      url %>%
      xml2::read_html()
    
    html_table <-
      html %>%
      rvest::html_table(fill = TRUE, header = FALSE) %>%
      pluck(1) %>%
      as_tibble() %>%
      slice(3:n())
    
    if (is.null(col_names)) {
      col_names <-
        html %>%
        rvest::html_table(fill = TRUE, header = FALSE) %>%
        pluck(1) %>%
        as_tibble() %>%
        slice(1) %>%
        as.character()
    }
    html_table %>%
      set_names(col_names)
  }

.clean_wintotals_nfl_sportsoddshistory <-
  function(data, ...) {
    data %>%
      select(matches("tm|season")) %>%
      gather(lab_season, ou_record, matches("season")) %>%
      separate(lab_season,
               into = c("lab", "season"),
               sep = "_") %>%
      select(-matches("^lab")) %>%
      mutate_at(vars(season), funs(as.integer)) %>%
      filter(season != max(season)) %>%
      separate(ou_record,
               into = c("w", "w_sportsbook", "result"),
               sep = "\\s+") %>%
      mutate_at(vars(w, w_sportsbook), funs(as.integer)) %>%
      select(season, tm_name_full, w, w_sportsbook, result)
  }

.recode_tm_col_nfl_sportsoddshistory <-
  function(data, col_join = "tm_name_full", ...) {
    .recode_tm_col_nfl_cautiously_at(data = data, col_join = col_join, ...)
  }

do_get_wintotals_nfl_sportsoddshistory <-
  function(season = config$season_current_nfl, ...) {
    .f_possibly <-
      purrr::possibly(.get_wintotals_nfl_sportsoddshistory, otherwise = tibble())
    res <-
      .f_possibly(...) %>%
      .clean_wintotals_nfl_sportsoddshistory(...) %>%
      .recode_tm_col_nfl_sportsoddshistory(...) %>%
      .reorder_cols_nfl_at(...)
    res
  }
