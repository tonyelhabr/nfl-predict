
.URL_FL_LINES_LATEST <- "http://www.footballlocks.com/nfl_lines.shtml"
# grid ----

.get_grid_pre_nfl_fl <-
  function(...) {
    tribble(
      ~wk, ~url,
      1L, "http://www.footballlocks.com/nfl_odds_preseason_hall_of_fame_game.shtml#Closing%20NFL%20Odds%20Preseason%20Hall%20of%20Fame%20Game,%202006",
      2L, "http://www.footballlocks.com/nfl_odds_preseason_week_1.shtml",
      3L, "http://www.footballlocks.com/nfl_odds_preseason_week_2.shtml",
      4L, "http://www.footballlocks.com/nfl_odds_preseason_week_3.shtml",
      5L, "http://www.footballlocks.com/nfl_odds_preseason_week_4.shtml"
    ) %>% 
      mutate(seasontype = 1L)
  }

.get_grid_reg_nfl_fl <-
  function(...) {
    fmt <- "http://www.footballlocks.com/nfl_odds_week_%s.shtml#Closing%sNFL%sOdds%sWeek%s17,%s2006"
    sep <- "%20"
    tibble(wk = seq(1L, 17L, 1L)) %>%
      mutate(url = sprintf(fmt, wk, sep, sep, sep, sep, sep)) %>% 
      mutate(seasontype = 2L)
  }


.get_grid_post_nfl_fl <-
  function(...) {
    tribble(
      ~wk, ~url,
      1L, "http://www.footballlocks.com/nfl_odds_wild_card_playoff_games.shtml#Closing%20NFL%20Odds%20Wild%20Card%20Playoff%20Games,%202007",
      2L, "http://www.footballlocks.com/nfl_odds_divisional_playoff_games.shtml#Closing%20NFL%20Odds%20Divisional%20Playoff%20Games,%202007",
      3L, "http://www.footballlocks.com/nfl_odds_conference_championship_playoff_games.shtml#Closing%20NFL%20Odds%20Conference%20Championship%20Playoff%20Games,%202007",
      4L, "http://www.footballlocks.com/nfl_odds_pro_bowl.shtml#Closing%20NFL%20Odds%20Pro%20Bowl,%202007",
      5L, "http://www.footballlocks.com/nfl_odds_super_bowl.shtml#Closing%20NFL%20Odds%20Super%20Bowl%20XLI%20(41),%202007"
    ) %>% 
      mutate(seasontype = 3L)
  }

.get_grid_nfl_fl <-
  function(...) {
    bind_rows(
      .get_grid_pre_nfl_fl(),
      .get_grid_reg_nfl_fl(),
      .get_grid_post_nfl_fl()
    ) %>% 
      select(seasontype, wk, url) %>% 
      arrange(seasontype, wk)
  }

# do-functions ----
# NOTE: \r is added to the html when the page is downloaded.
.extract_tr_nfl_fl <-
  function(html) {
    html %>% 
      rvest::html_nodes("table tr") %>% 
      rvest::html_text() %>% 
      as.list() %>% 
      str_subset("[05] ET[\r\n]") %>% 
      str_subset("(?!(reference).).*$") %>% 
      str_trim()
  }

.clean_tr_nfl_fl <-
  function(x) {
    x %>% 
      str_replace_all("\n", " ") %>% 
      str_replace_all("\\s+", " ")
  }


..filter_bylength <-
  function(x, max = 80) {
    ifelse(nchar(x) > max, NA, x)
    # NOTE: This doesn't work as intended.
  }

.filter_tr_nfl_fl <-
  function(x) {
    res <- ..filter_bylength(x)
    res[!is.na(res)]
  }

.filter_lines_nfl_fl <-
  function(data, ...) {
    data %>% 
      .extract_tr_nfl_fl() %>% 
      .clean_tr_nfl_fl() %>% 
      .filter_tr_nfl_fl() %>% 
      as_tibble()
  }

# ..separate_cols_nfl_fl <-
#   function(data) {
#     data %>%
#       separate(value, into = c("time", "gm"), sep = "\\s+ET\\s+") %>%
#       separate(time,  into = c("date", "time"), sep = "\\s+")
#   }
# 
# ..add_cols_tm_nfl_fl <-
#   function(data) {
#     data %>% 
#       mutate(tm_home_isfav = if_else(str_detect(gm, "^At"), TRUE, FALSE)) %>%
#       mutate(tm_home = str_replace(gm, "(^.*At\\s+)([A-Za-z\\s]+)(\\s[\\-?0-9].*$)", "\\2")) %>%
#       mutate(tm_away = 
#                str_extract_all(gm, "([A-Za-z\\s]+)") %>% 
#                purrr::map2_chr(ifelse(tm_home_isfav, 2, 1), ~.[[.y]]))
#   }
# 
# ..add_col_spread_nfl_fl <-
#   function(data) {
#     data %>%
#       mutate(spread = str_replace(gm, "(^.*[a-z]\\s+)(\\-?[0-9.]+)(\\s+[A-Z].*$)", "\\2"))
#   }
# 
# ..add_col_total_nfl_fl <-
#   function(data) {
#     data %>%
#       mutate(total = str_replace_all(gm, "(^.*)(\\s[0-9.]+)$", "\\2"))
#   }
# 
# ..clean_cols_all_nfl_fl <-
#   function(data) {
#     data %>%
#       mutate_if(is.character, str_trim)
#   }
# 
# ..clean_cols_lines_nfl_fl <-
#   function(data) {
#     data %>%
#       mutate_at(vars(spread, total), funs(as.numeric)) %>% 
#       mutate_at(vars(spread), funs(if_else(tm_home_isfav, ., -.)))
#   }
# 
# ..add_cols_date_nfl_fl <-
#   function(data) {
#     data %>%
#       mutate(date = sprintf("%s/%s", date, format(Sys.Date(), "%Y")) %>% lubridate::mdy()) %>% 
#       mutate(time = lubridate::ymd_hm(paste0(date, " ", time)))
#   }
# 
# .clean_xxx_nfl_fl <-
#   function(data, ...) {
#     data %>%
#       ..separate_cols_nfl_fl() %>%
#       ..add_cols_tm_nfl_fl() %>%
#       ..add_col_spread_nfl_fl() %>%
#       ..add_col_total_nfl_fl() %>%
#       ..clean_cols_all_nfl_fl() %>%
#       ..clean_cols_lines_nfl_fl() %>%
#       ..add_cols_date_nfl_fl()
#   }

.clean_xxx_nfl_fl <-
  function(data, ...) {
    data %>%
      separate(value, into = c("time", "gm"), sep = "\\s+ET\\s+") %>%
      separate(time,  into = c("date", "time"), sep = "\\s+") %>% 
      mutate_at(vars(gm), funs(str_replace(., "\\sPK\\s", " -0.0 "))) %>% 
      mutate(tm_home_isfav = if_else(str_detect(gm, "^At"), TRUE, FALSE)) %>%
      mutate(tm_home = str_replace(gm, "(^.*At\\s+)([A-Za-z\\s]+)(\\s[\\-?0-9].*$)", "\\2")) %>%
      mutate(tm_away = 
               str_extract_all(gm, "([A-Za-z\\s]+)") %>% 
               purrr::map2_chr(ifelse(tm_home_isfav, 2, 1), ~.[[.y]])) %>% 
      mutate(spread = str_replace(gm, "(^.*[a-z]\\s+)(\\-?[0-9.]+)(\\s+[A-Z].*$)", "\\2")) %>%
      mutate(total = str_replace_all(gm, "(^.*)(\\s[0-9.]+)$", "\\2")) %>% 
      mutate_if(is.character, str_trim) %>% 
      mutate_at(vars(spread, total), funs(as.numeric)) %>% 
      mutate_at(vars(spread), funs(if_else(tm_home_isfav, ., -.))) %>% 
      mutate(date = sprintf("%s/%s", date, format(Sys.Date(), "%Y")) %>% lubridate::mdy()) %>% 
      mutate(time = lubridate::ymd_hm(paste0(date, " ", time))) 
  }

.clean_lines_nfl_fl <-
  function(data, ...) {
    .clean_xxx_nfl_fl(
      data = data,
      ...
    ) %>% 
      select(-gm, -tm_home_isfav)
  }

.clean_odds_nfl_fl <-
  function(data, ...) {
    # fl_raw %>%
    data %>% 
      mutate(moneylines = str_replace(value, "(^.*)([+-]\\$.*[+-]\\$.*$)", "\\2")) %>% 
      separate(moneylines, into = c("moneyline1", "moneyline2"), sep = "\\s") %>% 
      mutate_at(vars(matches("moneyline")), funs(str_replace(., "\\$", "") %>% as.integer())) %>% 
      mutate_at(vars(value), funs(str_remove(., "\\s[+-]\\$[0-9]+\\s[+-].*$"))) %>% 
      .clean_xxx_nfl_fl() %>% 
      mutate(moneyline_home = if_else(str_detect(gm, "^At"), moneyline1, moneyline2)) %>% 
      select(-matches("moneyline[12]")) %>% 
      select(-gm, tm_home_isfav)
      
  }

.recode_tm_cols_fl <-
  function(data, ...) {
    .recode_tm_cols_at(
      data = data,
      col = "tm_name_fl"
    )
  }

.finalize_lines_nfl_fl <-
  function(data, ...) {
    data %>%
      .recode_tm_cols_fl(...) %>% 
      .add_timeperiod_cols_nfl(...) %>% 
      .arrange_gm_nfl(...)
  }

# do ----
do_get_lines_nfl_fl <-
  function(url = NULL, ...) {
    if(is.null(url)) {
      url <- .URL_FL_LINES_LATEST
      msg <- sprintf("Setting `url` = \"%s\" since none is specified.", url)
      message(msg)
    }
    html <- xml2::read_html(url)
    html %>% 
      .filter_lines_nfl_fl(...) %>% 
      .clean_lines_nfl_fl(...) %>% 
      .finalize_lines_nfl_fl(...)
  }

do_get_lines_latest_nfl_fl <-
  function(url = .URL_FL_LINES_LATEST, ...) {
    do_get_lines_nfl_fl(url = url, ...)
  }
