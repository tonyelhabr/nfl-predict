

.COL_ESPN <- "name"
.RGX_SPLIT_ESPN <- "\\."

# grid ----
# NOTE: This is "hard-coded", although it could be inferred from the ESPN data.
.get_seasontypeid_nfl_espn <- function(...) {
  tribble(~ seasontypeid,
          ~ seasontype,
          1L,
          "Preseason",
          2L,
          "Regular Season",
          3L,
          "Post Season")
}

.get_grid_nfl_espn <-
  function(season = as.integer(format(Sys.Date(, "%Y"))), ...) {
    grid <-
      expand.grid(
        season = season,
        seasontype = seq(1, 3, 1),
        wk = seq(1, 17, 1)
      ) %>%
      as_tibble() %>%
      mutate_all(as.integer) %>%
      arrange(season, seasontype, wk) %>%
      filter((seasontype == 1L &
                wk <= 5L) | (seasontype == 2L) | (seasontype == 3L & wk <= 5L))
    
    grid %>%
      # NOTE: I believe espn treats the hof game as the same week as preseason week 1
      # before 2008 (meaning that that there is only 4 weeks for seasontype = 1.
      mutate(before2008 = if_else(season < 2008 &
                                    seasontype == 1 & wk == 5, TRUE, FALSE)) %>%
      # NOTE: espn actually has a score of 0-0 for the 2016 game that was cancelled, but does
      # not have anything recorded for the 2011 game that was cancelled.
      mutate(nohofgm = if_else(season == 2011 |
                                 season == 2016, TRUE, FALSE)) %>%
      # NOTE: espn does not appear to record the probowl scores after 2010.
      mutate(noprobowl = if_else(season > 2010 &
                                   seasontype == 3 & wk == 4, TRUE, FALSE)) %>%
      filter(!before2008 & !nohofgm & !noprobowl) %>%
      select(-before2008, -nohofgm, -noprobowl)
    
  }

# filter ----
.get_cols_max_espn_at <-
  function(...,
           col = .COL_ESPN,
           rgx_split = .RGX_SPLIT_ESPN) {
    .get_cols_max_at(..., col = col, rgx_split = rgx_split)
  }

# NOTE: When separating, `seasontype = 3` games seem to have 8 columns instead of just 7.
.separate_cols_max_espn_at <-
  function(...,
           col = .COL_ESPN,
           rgx_split = .RGX_SPLIT_ESPN) {
    .separate_cols_max_at(..., col = col, rgx_split = rgx_split)
  }

# filter ----
.filter_scores_nfl_espn <-
  function(data, ...) {
    # browser()
    data_sep <-
      data %>%
      .separate_cols_max_espn_at(col = "name")
    # if(ncol(data_sep) < 6) {
    #   msg <- "No games?"
    #   message(msg)
    #   return(data)
    # }
    data_sep %>%
      filter((name1 == "events" &
                name2 == "shortName") |
               (name1 == "events" &
                  name2 == "competitions" &
                  name3 == "date") | (
                    name1 == "events" &
                      name2 == "competitions" &
                      name3 == "status" &
                      name4 == "type" &
                      name5 == "name"
                  ) |
               (
                 name1 == "events" &
                   name2 == "competitions" &
                   name3 == "competitors" &
                   name4 == "score"
               )
      )
  }

# clean ----
.clean_scores_nfl_espn <-
  function(data, ...) {
    data %>%
      select(name3, name4, name5, value) %>%
      mutate(status = if_else(name5 == "name", value, NA_character_)) %>%
      mutate(isscore = if_else(name4 == "score", TRUE, FALSE)) %>%
      mutate(datetime = if_else(
        name3 == "date",
        str_replace_all(value, "\\s?T\\s?", " ") %>% str_replace("Z$", ""),
        NA_character_
      )) %>%
      mutate(gm = if_else(
        is.na(isscore) &
          is.na(datetime) & is.na(status),
        value,
        NA_character_
      )) %>%
      fill(status, .direction = "up") %>%
      filter(status == "STATUS_FINAL") %>%
      fill(gm, .direction = "down") %>%
      fill(datetime, .direction = "down") %>%
      filter(name3 == "competitors") %>%
      group_by(gm) %>%
      mutate(rn = row_number()) %>%
      ungroup() %>%
      mutate(tm_dir = if_else(rn == 1, "pts_home", "pts_away")) %>%
      select(datetime, gm, tm_dir, value) %>%
      spread(tm_dir, value) %>%
      separate(gm, into = c("tm_away", "tm_home"), sep = "(\\s+\\@\\s+)|(\\s+vs.*\\s+)") %>%
      mutate_at(vars(matches("pts")), funs(as.integer)) %>%
      mutate(date = datetime %>% str_remove("\\s.*$") %>% lubridate::ymd()) %>%
      mutate(time = datetime %>% lubridate::ymd_hm()) %>%
      select(date, time, tm_home, tm_away, matches("pts"))
  }

# postprocess ----
.recode_tm_cols_espn <-
  function(data, ...) {
    .recode_tm_cols_strictly_at(data = data,
                       col = "tm_espn")
  }

.finalize_xxx_nfl_espn <-
  function(data,
           ...,
           season = config::get()$season_current,
           arrange = TRUE) {
    res <-
      data %>%
      .recode_tm_cols_espn(...)
    # .add_timeperiod_cols_nfl(...)
    
    if (arrange) {
      res <-
        .arrange_gm_nfl(..., season = season)
    }
    res
  }

.fix_wk_scores_nfl_espn <-
  # function(data, season, seasontype, wk, ...) {
  function(data, ...) {
    # NOTE: espn considers seasontype = 3, wk = 5 to be the probowl for seasons before 2008
    # (and wk = 4 to be the superbowl)
    data %>%
      mutate(temp = if_else(season < 2008L &
                              seasontype == 3L & wk == 4L, TRUE, FALSE)) %>%
      mutate(wk = if_else(season < 2008L &
                            seasontype == 3L & wk == 5L, 4L, wk)) %>%
      mutate(wk = if_else(temp, 5L, wk)) %>%
      select(-temp)
    # NOTE: Not sure how to "re-arrange" here.
  }

# do ----
.preprocess_do_get_xxx_nfl_espn <-
  function(wk,
           season = config::get()$season_current,
           ...,
           seasontype = 2L) {
    .season <- season
    .seasontype <- seasontype
    .wk <- wk
    # .season <- 2006L
    # .seasontype <- 2L
    # .wk <- 1L:3L
    # NOTE: Technically, `season` doesn't need to be renamed.
    grid <-
      .get_grid_nfl_espn(season = .season) %>%
      filter(seasontype %in% .seasontype) %>%
      filter(wk %in% .wk)
    
    data_raw <-
      grid %>%
      mutate(data = purrr::pmap(
        list(season, seasontype, wk),
        ~ espn2::get_scores_nfl(
          week = ..3,
          year = ..1,
          seasontype = ..2,
          as = "minimal"
        )
      ))
  }

do_get_scores_nfl_espn <-
  function(wk,
           season = config::get()$season_current,
           seasontype = 2L,
           ...,
           arrange = ifelse(season ==config::get()$season_current, TRUE, FALSE)) {
    res <-
      .preprocess_do_get_xxx_nfl_espn(
        wk = wk,
        season = season,
        seasontype = seasontype,
        arrange = arrange,
        ...
      )
    
    res <-
      res %>% mutate(data = purrr::map(data, ~ .filter_scores_nfl_espn(data = .x, ...)))
    res <-
      res %>% mutate(data = purrr::map(data, ~ .clean_scores_nfl_espn(data = .x, ...)))
    res <-
      res %>%
      mutate(data = purrr::map2(
        data,
        season,
        ~ .finalize_xxx_nfl_espn(
          data = .x,
          season = .y,
          arrange = arrange,
          ...
        )
      )) %>%
      unnest() %>%
      .fix_wk_scores_nfl_espn(...) %>% 
      .reorder_cols_nfl_at(...)
    
  }

do_get_scores_season_nfl_espn <-
  function(season = config::get()$season_current,
           ...,
           seasontype = 1L:3L,
           wk = 1L:17L,
           arrange = FALSE) {
    res <-
      do_get_scores_nfl_espn(
        season = season,
        seasontype = seasontype,
        wk = wk,
        arrange = arrange,
        ...
      )
  }
