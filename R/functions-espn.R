
.COL_ESPN <- "name"
.RGX_SPLIT_ESPN <- "\\."

# preprocess ----
.get_cols_max_espn_at <-
  function(..., col = .COL_ESPN, rgx_split = .RGX_SPLIT_ESPN) {
    .get_cols_max_at(..., col = col, rgx_split = rgx_split)
  }

# NOTE: When separating, `seasontype = 3` games seem to have 8 columns instead of just 7.
.separate_cols_max_espn_at <-
  function(..., col = .COL_ESPN, rgx_split = .RGX_SPLIT_ESPN) {
    .separate_cols_max_at(..., col = col, rgx_split = rgx_split)
  }


# postproces ----
.recode_tm_cols_espn <-
  function(data, ...) {
    .recode_tm_cols_at(
      data = data,
      col = "tm_espn"
    )
  }


# main ----
.filter_scores_nfl_espn <-
  function(data, ...) {
    # browser()
    data_sep <-
      data %>%
      .separate_cols_max_espn_at(col = "name")
    if(ncol(data_sep) < 6) {
      msg <- "No games?"
      message(msg)
      return(data)
    }
    data_sep %>% 
      filter((name1 == "events" &
        name2 == "shortName") |
          (name1 == "events" &
             name2 == "competitions" &
             name3 == "date"
          ) | (
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
        ))
  }


# .filter_odds_nfl_espn <-
#   function(data, ...) {
#     data %>%
#       .separate_cols_max_espn_at(col = "name") %>%
#       filter((name1 == "events" &
#         name2 == "shortName") |
#           (name1 == "events" &
#              name2 == "competitions" &
#              name3 == "date"
#           ) |
#         (
#           name1 == "events" &
#             name2 == "competitions" &
#             name3 == "odds" &
#             name4 %in% c("details", "overUnder")
#         ))
#   }

.clean_scores_nfl_espn <-
  function(data, ...) {
    # browser()
    data %>%
      select(name3, name4, name5, value) %>%
      mutate(status = if_else(name5 == "name", value, NA_character_)) %>% 
      mutate(isscore = if_else(name4 == "score", TRUE, FALSE)) %>%
      mutate(datetime = if_else(name3 == "date", str_replace_all(value, "\\s?T\\s?", " ") %>% str_replace("Z$", ""), NA_character_)) %>% 
      mutate(gm = if_else(is.na(isscore) & is.na(datetime) & is.na(status), value, NA_character_)) %>%
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

# .clean_odds_nfl_espn <-
#   function(data, ...) {
#     data %>%
#       select(isodds = name3, odds_type = name4, value) %>%
#       mutate_at(vars(isodds), funs(if_else(is.na(.), FALSE, TRUE))) %>%
#       mutate(gm = if_else(!isodds, value, NA_character_)) %>%
#       fill(gm, .direction = "down") %>%
#       filter(gm != value) %>%
#       select(gm, odds_type, value) %>%
#       spread(odds_type, value) %>%
#       separate(gm, into = c("tm_away", "tm_home"), sep = "(\\s+\\@\\s+)|(\\s+vs.*\\s+)") %>%
#       separate(details, into = c("tm_favored", "spread"), sep = "\\s+") %>%
#       mutate_at(vars(spread, overUnder), funs(as.numeric)) %>%
#       mutate(spread_home = if_else(tm_home == tm_favored, spread, -spread)) %>%
#       select(-tm_favored, -spread) %>%
#       rename(total = overUnder) %>%
#       mutate_at(vars(total), funs(if_else(. <= 0, NA_real_, .)))
#   }

.finalize_xxx_nfl_espn <-
  function(data, ..., season = config::get()$season_current, arrange = TRUE) {
    .season <- season
    res <-
      data %>%
      .recode_tm_cols_espn(...)
    # .add_timeperiod_cols_nfl(...)
    
    if(arrange) {
      res <-
        .arrange_gm_nfl(..., season = .season)
    }
    res
  }

# grid ----
# NOTE: This is "hard-coded", although it could be inferred from the ESPN data.
.get_seasontypeid_nfl_espn <- function(...) {
  tribble(
    ~seasontypeid, ~seasontype,
    1L, "Preseason",
    2L, "Regular Season",
    3L, "Post Season"
  )
}

.get_grid_nfl_espn <-
  function(season = as.integer(format(Sys.Date(, "%Y"))), ...) {
    expand.grid(
      season = season,
      seasontype = seq(1, 3, 1),
      wk = seq(1, 17, 1)
    ) %>%
      as_tibble() %>%
      mutate_all(as.integer) %>%
      filter((seasontype == 1L &
        wk <= 5L) |
        (seasontype == 2L) | (seasontype == 3L & wk <= 5L)) %>%
      arrange(season, seasontype, wk)
  }

# do ----
.preprocess_do_get_xxx_nfl_espn <-
  function(wk, season = config::get()$season_current, ..., seasontype = 2L) {
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
        ~espn2::get_scores_nfl(
          week = ..3,
          year = ..1,
          seasontype = ..2,
          as = "minimal"
        )
      ))
  }

.postprocess_do_get_xxx_nfl_espn <-
  function(data, arrange, ...) {
    data %>%
      mutate(data = purrr::map2(data, season, ~.finalize_xxx_nfl_espn(data = .x, season = .y, arrange = arrange, ...))) %>%
      unnest()
  }
# .get_path_export_scores_nfl_espn <-
#   function(season, seasontype, wk, ...) {
#     file.path("data-raw", paste0("scores-", season, "-espn-", format(Sys.time(), "%F_%T"), ".csv"))
#     # file.path("data-raw", paste0("scores-", season, "-espn.csv"))
#   }

do_get_scores_nfl_espn <-
  function(wk, season = config::get()$season_current, seasontype = 2L, ..., arrange = TRUE) {
    res <-
      .preprocess_do_get_xxx_nfl_espn(
        wk = wk,
        season = season,
        seasontype = seasontype,
        arrange = arrange,
        ...
      )

    res <- res %>% mutate(data = purrr::map(data, ~.filter_scores_nfl_espn(data = .x, ...)))
    res <- res %>% mutate(n_col = purrr::map(data, ~ncol(.x)))
    n_row1 <- nrow(res)
    res <- res %>% filter(n_col > 6)
    n_row2 <- nrow(res)
    if(n_row2 < n_row1) {
      msg <- sprintf("Could not process %.0f season-wk-seasontype combinations for `season` = %.0.f", (n_row1 - n_row2), season)
      message(msg)
    }
    if(n_row2 == 0) {
      msg <- sprintf("Nothing processed.")
      warning(msg)
      return()
    }
    res <- res %>%  mutate(data = purrr::map(data, ~.clean_scores_nfl_espn(data = .x, ...)))
    res <- res %>% .postprocess_do_get_xxx_nfl_espn(..., arrange = arrange)
    res
  }

do_get_scores_season_nfl_espn <-
  function(season = config::get()$season_current, ..., seasontype = 1L:3L, wk = 1L:17L, arrange = FALSE) {
    do_get_scores_nfl_espn(
      season = season,
      seasontype = seasontype,
      wk = wk,
      arrange = arrange,
      ...
    )
  }

# do_get_odds_nfl_espn <-
#   function(wk, season = config::get()$season_current, seasontype = 2L, ..., arrange = TRUE) {
#     data_raw <-
#       .preprocess_do_get_xxx_nfl_espn(
#         wk = wk,
#         season = season,
#         seasontype = seasontype,
#         ...
#       )
# 
#     data_raw %>%
#       mutate(data = purrr::map(data, ~.filter_scores_nfl_espn(data = .x, ...))) %>%
#       mutate(data = purrr::map(data, ~.clean_scores_nfl_espn(data = .x, ...))) %>%
#       .postprocess_do_get_xxx_nfl_espn(..., arrange = arrange)
#   }
