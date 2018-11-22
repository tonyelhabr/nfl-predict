

# grid ----
.get_grid_pre_nfl_fl <-
  function(...) {
    tribble(
      ~ wk,
      ~ url,
      1L,
      "http://www.footballlocks.com/nfl_odds_preseason_hall_of_fame_game.shtml#Closing%20NFL%20Odds%20Preseason%20Hall%20of%20Fame%20Game,%202006",
      2L,
      "http://www.footballlocks.com/nfl_odds_preseason_week_1.shtml",
      3L,
      "http://www.footballlocks.com/nfl_odds_preseason_week_2.shtml",
      4L,
      "http://www.footballlocks.com/nfl_odds_preseason_week_3.shtml",
      5L,
      "http://www.footballlocks.com/nfl_odds_preseason_week_4.shtml"
    ) %>%
      mutate(seasontype = 1L)
  }

.get_grid_reg_nfl_fl <-
  function(...) {
    fmt <-
      "http://www.footballlocks.com/nfl_odds_week_%s.shtml#Closing%sNFL%sOdds%sWeek%s17,%s2006"
    sep <- "%20"
    tibble(wk = seq(1L, 17L, 1L)) %>%
      mutate(url = sprintf(fmt, wk, sep, sep, sep, sep, sep)) %>%
      mutate(seasontype = 2L)
  }


.get_grid_post_nfl_fl <-
  function(...) {
    tribble(
      ~ wk,
      ~ url,
      1L,
      "http://www.footballlocks.com/nfl_odds_wild_card_playoff_games.shtml#Closing%20NFL%20Odds%20Wild%20Card%20Playoff%20Games,%202007",
      2L,
      "http://www.footballlocks.com/nfl_odds_divisional_playoff_games.shtml#Closing%20NFL%20Odds%20Divisional%20Playoff%20Games,%202007",
      3L,
      "http://www.footballlocks.com/nfl_odds_conference_championship_playoff_games.shtml#Closing%20NFL%20Odds%20Conference%20Championship%20Playoff%20Games,%202007",
      4L,
      "http://www.footballlocks.com/nfl_odds_pro_bowl.shtml#Closing%20NFL%20Odds%20Pro%20Bowl,%202007",
      5L,
      "http://www.footballlocks.com/nfl_odds_super_bowl.shtml#Closing%20NFL%20Odds%20Super%20Bowl%20XLI%20(41),%202007"
    ) %>%
      mutate(seasontype = 3L)
  }

# NOTE: This function does not take `season` as an input, like the analogous espn function.
.get_grid_nfl_fl <-
  function(...) {
    bind_rows(.get_grid_pre_nfl_fl(),
              .get_grid_reg_nfl_fl(),
              .get_grid_post_nfl_fl()) %>%
      select(seasontype, wk, url) %>%
      arrange(seasontype, wk)
  }

# filter ----
# NOTE: \r is added to the html when the page is downloaded.
.RGX_ODDS_HISTORICAL_FL <- "(Closing\\sLas)|(\\<TD\\>)"
.RGX_ODDS_LATEST_FL <- "(NFL\\sFootball\\sOdds$)|(\\<TD\\>)"
.filter_odds_nfl_fl <-
  function(path, rgx = .RGX_ODDS_HISTORICAL_FL, ...) {
    path %>%
      readLines(warn = FALSE) %>%
      str_replace_all("[\\r\\n]+", "") %>%
      # NOTE: Replace "&nbsp".
      # str_replace_all("\u00A0", " ") %>%
      str_replace_all("\\&nbsp;", " ") %>%
      str_replace_all("\\s+", " ") %>%
      # NOTE: Only want these lines.
      str_subset(rgx) %>%
      # NOTE: Remove quotes.
      str_replace_all("(^.*\\\")(.*)(\\\".*$)", "\\2") %>%
      # NOTE: Remove "TD" tags.
      str_replace_all("(\\<\\/?TD\\>)", "") %>%
      # NOTE: There are some "D" tags (for some unknown reason). Also, there are "I" tags (for postponed games).
      str_replace_all("(\\<\\/?D\\>)|(\\<\\/?I\\>)|(\\<\\/?[Bb][Rr]\\>)", "") %>%
      # NOTE: Don't Want the lines that start with "font size", even if they indicate that the game is played outside the US.
      # These would be difficult to handle with the current logic.
      str_subset("(?!\\<[Ff][Oo][Nn][Tt].*)") %>%
      str_trim() %>%
      as_tibble()
  }

# clean ----
# NOTE: These are defined in their own variables only because they are relatively long.
.RGX_CITY_NONUS_FL <- "London|Wembley|Mexico\\sCity"
.RGX_REPLACE_CITY_NONUS_FL <-
  paste0(
    "(\\<[Ff][Oo][Nn][Tt].*\\s[Ss][Ii][Zz][Ee]\\=\\-?[0-9]+\\>)|(At\\s+",
    .RGX_CITY_NONUS_FL,
    ")|(",
    .RGX_CITY_NONUS_FL,
    ")"
  )
.clean_odds_raw_nfl_fl <-
  function(data, ...) {
    # browser()
    res <-
      data %>%
      # NOTE: Not sure why, but the earlier cleaning for these values does not seem to work?
      mutate_at(vars(value),
                funs(
                  str_replace_all(., "\\&nbsp;", " ") %>% str_replace_all("\\s+", " ") %>% str_trim()
                )) %>%
      filter(!str_detect(value, "Postponed")) %>%
      mutate(value = if_else(
        str_detect(value, .RGX_CITY_NONUS_FL),
        paste0("At ", value) %>%
          str_replace_all(.RGX_REPLACE_CITY_NONUS_FL, ""),
        value
      )) %>%
      mutate(value = if_else(str_detect(value, "Pick Em|(PK)"), "-0", value)) %>%
      mutate(label = if_else(str_detect(value, "Closing\\sLas"), value, NA_character_)) %>%
      mutate(datetime = if_else(str_detect(value, "[0-9]+\\/"), value, NA_character_)) %>%
      mutate(
        key =
          case_when(
            str_detect(value, "^At\\s") ~ "tm_home",
            str_detect(value, "^[A-Za-z]") ~ "tm_away",
            str_detect(value, "^\\-[0-9]") ~ "spread",
            str_detect(value, "^[0-9\\.]+$") ~ "total",
            str_detect(value, "^[-+]\\$") ~ "moneylines",
            TRUE ~ NA_character_
          )
      ) %>%
      mutate(
        yr = if_else(
          !is.na(label),
          # NOTE: Conference Championship year label has a trailing comma.
          str_replace_all(value, "(^.*,\\s)([0-9]+)", "\\2") %>% str_replace_all("\\,", ""),
          NA_character_
        ),
        wk = if_else(
          !is.na(label),
          case_when(
            str_detect(value, "Week") ~ str_replace_all(value, "(^.*Week\\s)(.*)(\\,.*$)", "\\2"),
            str_detect(value, "Wild") ~ "1",
            str_detect(value, "Divisional") ~ "2",
            str_detect(value, "Conference") ~ "3",
            str_detect(value, "Pro") ~ "4",
            str_detect(value, "Super") ~ "5",
            # NOTE: `1L` will be added to preseason weeks in a later step.
            str_detect(value, "Hall") ~ "0",
            TRUE ~ NA_character_
          ),
          NA_character_
        ),
        seasontype = if_else(
          !is.na(label),
          case_when(
            str_detect(value, "Hall|Pre") ~ "1",
            str_detect(value, "Week") ~ "2",
            TRUE ~ "3"
          ),
          NA_character_
        )
      ) %>%
      mutate(tm_home_isfav = if_else(key == "spread", if_else(lag(key == "tm_home"), TRUE, FALSE), NA)) %>%
      mutate_at(vars(tm_home_isfav), funs(if_else(!is.na(lead(
        ., 1
      )), lead(., 1), .))) %>%
      fill(tm_home_isfav, .direction = "down") %>%
      fill(yr, .direction = "down") %>%
      fill(wk, .direction = "down") %>%
      fill(seasontype, .direction = "down") %>%
      filter(is.na(label)) %>%
      select(-label) %>%
      mutate(grp = if_else(!is.na(datetime), row_number(), NA_integer_)) %>%
      fill(grp, .direction = "down") %>%
      fill(datetime, .direction = "down") %>%
      filter(value != datetime) %>% 
      mutate_at(vars(key), funs(
        case_when(
          # NOTE: Case for Super Bowl/Pro Bowl.
          seasontype == "3" &
            wk %in% c("4", "5") &
            key == "tm_away" & lead(key, 2) == "tm_away" ~ "tm_home",
          # NOTE: Case for some non-US games that aren't parsed correctly before.
          key == "tm_away" &
            lead(key, 2) == "tm_away" ~ "tm_home",
          TRUE ~ .
        )
      ))
    res
  }

.spread_odds_raw_nfl_fl <-
  function(data, ...) {
    data %>%
      group_by(grp, key) %>%
      mutate(n = row_number()) %>%
      ungroup() %>%
      filter(n == 1) %>%
      select(-n) %>%
      spread(key, value)
  }

.clean_odds_wide_nfl_fl <-
  function(data, ...) {
    data %>%
      mutate_at(vars(moneylines), funs(str_replace_all(., "\\&nbsp\\;", ""))) %>%
      mutate_at(vars(spread), funs(if_else(
        !tm_home_isfav, str_replace(., "\\-", "+"), .
      ))) %>%
      rename(spread_home = spread) %>%
      separate(moneylines,
               into = c("moneyline1", "moneyline2"),
               sep = "\\s+") %>%
      mutate_at(vars(matches("moneyline")), funs(str_replace(., "\\$", ""))) %>%
      mutate(
        moneyline_home = if_else(tm_home_isfav, moneyline1, moneyline2),
        moneyline_away = if_else(tm_home_isfav, moneyline2, moneyline1)
      ) %>%
      select(-matches("moneyline[12]")) %>%
      mutate_at(vars(matches("yr|wk|seasontype|moneyline")), funs(as.integer)) %>%
      mutate_at(vars(wk), funs(if_else(seasontype == 1L, . + 1L, .))) %>% 
      mutate(season = yr) %>% 
      mutate_at(vars(season), funs(
        case_when(
          # NOTE: Using `case_when()` because there may be other cases not considered here that need to be implemented.
          seasontype < 3 ~ . + 1L,
          TRUE ~ .
        )
      )
      ) %>% 
      mutate_at(vars(spread_home, total), funs(as.numeric)) %>%
      mutate_at(vars(datetime), funs(str_replace_all(., "\\s+ET.*$", ""))) %>%
      separate(datetime, into = c("date", "time"), sep = "\\s+") %>%
      mutate(date = lubridate::mdy(sprintf("%s/%s", date, yr))) %>%
      mutate(time = lubridate::ymd_hm(sprintf("%s %s", date, time))) %>%
      mutate_at(vars(tm_home), funs(str_replace_all(., "^At\\s+", ""))) %>%
      arrange(season, seasontype, wk, date, time, tm_home, tm_away) %>%
      mutate_at(vars(grp), funs(row_number(.))) %>%
      select(
        season,
        seasontype,
        wk,
        date,
        time,
        tm_home,
        tm_away,
        spread_home,
        total,
        moneyline_home,
        moneyline_away
      )
  }

.do_clean_odds_nfl_fl <-
  function(data, ...) {
    res <-
      data %>%
      .clean_odds_raw_nfl_fl(...)
    res <-
      res %>%
      .spread_odds_raw_nfl_fl(...)
    res <-
      res %>%
      .clean_odds_wide_nfl_fl(...)
    res
  }


# postprocess ----
.recode_tm_cols_nfl_fl <-
  function(data, col = "tm_name_fl", ...) {
    .recode_tm_cols_nfl_cautiously_at(data = data, col = col, ...)
  }

.postprocess_odds_nfl_fl <-
  function(data, ..., .arrange = TRUE, .season) {
    res <-
      data %>%
      .recode_tm_cols_nfl_fl(...)

    if (.arrange) {
      res <-
        res %>% 
        .arrange_gm_nfl(..., .season = .season)
    }
    res
  }

# do ----
..get_path_nfl_fl <-
  function(url, ...) {
    file.path("data-raw", "fl", paste0(
      str_replace_all(url, "(^.*com\\/)(.*)(\\.shtml.*$)", "\\2"),
      ".html"
    ))
  }

.add_col_path_nfl_fl <-
  function(data, download, ...) {
    if (!download) {
      res <- data %>% mutate(path = url)
    } else {
      res <-  data %>% mutate(path = ..get_path_nfl_fl(season))
    }
    res
  }

.preprocess_do_get_odds_nfl_fl <-
  function(...,
           download = FALSE) {
    # NOTE: Technically, `season` doesn't need to be renamed.
    grid <- .get_grid_nfl_fl(...)
    grid <-
      grid %>%
      mutate(path = purrr::map_chr(url, ~ ..get_path_nfl_fl(.x)))
    
    if (download) {
      grid <-
        grid %>%
        mutate(dl =
                 purrr::walk2(url,
                              path,
                              ~ download.file(
                                url = .x,
                                destfile = .y,
                                quiet = TRUE
                              )))
    }
    
    grid <-
      grid %>% mutate(dl = if_else(file.exists(path), TRUE, FALSE))
    dls <- grid %>% pull(dl)
    if (!all(dls)) {
      msg <- sprintf("Missing some file.")
      stop(msg, call. = FALSE)
    }
    
    res <-
      grid %>%
      select(path)
    
    res
  }

.postprocess_do_get_xxx_nfl_fl <-
  function(data, .arrange, ...) {
    data %>%
      mutate(data = purrr::map2(
        data,
        season,
        ~ .postprocess_odds_nfl_fl(
          data = .x,
          season = .y,
          .arrange = .arrange,
          ...
        )
      ))
    
  }

# TODO: Need to check if these work!
do_get_odds_nfl_fl1 <-
  function(url,
           ...,
           rgx = .RGX_ODDS_LATEST_FL,
           path = NULL,
           download = ifelse(!is.null(path) &&
                               file.exists(path), FALSE, TRUE),
           .arrange = TRUE,
           season = config$season_current_nfl) {
    # html <- xml2::read_html(url)
    if (is.null(path)) {
      path <- ..get_path_nfl_fl(url)
    }
    if (download) {
      download.file(url, destfile = path, quiet = TRUE)
    }
    if (!file.exists(path)) {
      msg <- sprintf("File \"%s\" does not exist!", path)
      stop(msg, call. = FALSE)
    }
    # browser()
    # TODO: Figure out how to add week in here.
    res <-
      path %>%
      .filter_odds_nfl_fl(rgx = rgx, ...) %>%
      mutate_at(
        vars(value),
        funs(
          if_else(str_detect(., rgx), 
                  str_replace_all(., paste0("(^.*)(", rgx, ")"), paste0("Closing Las Vegas NFL Odds \\2, ", season)), .)
          )
      ) %>% 
      .do_clean_odds_nfl_fl(...) %>%
      select(-wk) %>% 
      .postprocess_odds_nfl_fl(.arrange = .arrange, season = season, ...)
    
    # NOTE: `wk` may be added back in after joining in `.postprocess_odds_nfl_fl()`.
    # If this is the case, reorder the columns
    if("wk" %in% names(res)) {
      res <-
        res %>% 
        select(season, seasontype, wk, everything())
    }
    res
  }

.URL_ODDS_LATEST_FL <- "http://www.footballlocks.com/nfl_odds.shtml"
do_get_odds_latest_nfl_fl <-
  function(url = .URL_ODDS_LATEST_FL, ...) {
    do_get_odds_nfl_fl1(url = url, ...)
  }

# do-historical ----
do_get_odds_nfl_fl <-
  function(wk,
           season = config$season_current_nfl,
           seasontype = 2L,
           ...,
           .arrange = ifelse(season == config$season_current_nfl, TRUE, FALSE)) {
    res <-
      .preprocess_do_get_odds_nfl_fl(...)
    
    res <-
      res %>% mutate(data = purrr::map(path, ~ .filter_odds_nfl_fl(path = .x, ...)))
    
    res <-
      res %>% mutate(data = purrr::map(data, ~ .do_clean_odds_nfl_fl(data = .x, ...)))
    
    .season <- season
    .seasontype <- seasontype
    .wk <- wk

    # NOTE: No need for a `.postprocess*()` function.
    res %>%
      unnest() %>% 
      filter(season %in% .season) %>% 
      filter(seasontype %in% .seasontype) %>% 
      filter(wk %in% .wk) %>% 
      .postprocess_odds_nfl_fl(season = season, .arrange = .arrange, ...) %>% 
      select(-path) %>%
      arrange(season, seasontype, wk, date, time, tm_home, tm_away)
    
  }

do_get_odds_season_nfl_fl <-
  function(season = (config$season_current_nfl - 1L),
           ...,
           seasontype = 1L:3L,
           wk = 1L:17L,
           .arrange = FALSE) {
    if(any(season == config$season_current_nfl)) {
      msg <- paste0(
        "This probably isn't going to work. Functionality to handle games in the current season ",
        "that are not in the current week has not yet been implemented. ",
        "To get the games in the current week, use the \"`_latest`\" function."
      )
      message(msg)
    }
    do_get_odds_nfl_fl(
      season = season,
      seasontype = seasontype,
      wk = wk,
      .arrange = .arrange,
      ...
    )
  }

.SEASON_MIN_FL <- 2006L
do_get_odds_all_nfl_fl <-
  function(season = .SEASON_MIN_FL:(config$season_current_nfl - 1L),
           ...) {
    do_get_odds_season_nfl_fl(season = season, ...)
  }
