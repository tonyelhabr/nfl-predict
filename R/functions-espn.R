
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

.clean_scores_nfl_espn <-
  function(data, ...) {
    # browser()
    data %>%
      select(isscore = name4, value) %>%
      mutate_at(vars(isscore), funs(if_else(is.na(.), FALSE, TRUE))) %>%
      mutate(gm = if_else(!isscore, value, NA_character_)) %>%
      fill(gm, .direction = "down") %>%
      filter(gm != value) %>%
      group_by(gm) %>%
      mutate(rn = row_number()) %>%
      ungroup() %>%
      mutate(tm_dir = if_else(rn == 1, "pts_home", "pts_away")) %>%
      select(gm, tm_dir, value) %>%
      spread(tm_dir, value) %>%
      separate(gm, into = c("tm_away", "tm_home"), sep = "(\\s+\\@\\s+)|(\\s+vs.*\\s+)") %>%
      mutate_at(vars(matches("pts")), funs(as.integer)) %>%
      select(tm_home, tm_away, matches("pts"))
  }

.clean_odds_nfl_espn <-
  function(data, ...) {
    data %>%
      select(isodds = name3, odds_type = name4, value) %>%
      mutate_at(vars(isodds), funs(if_else(is.na(.), FALSE, TRUE))) %>%
      mutate(gm = if_else(!isodds, value, NA_character_)) %>%
      fill(gm, .direction = "down") %>%
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

.finalize_xxx_nfl_espn <-
  function(data, ..., season = config::get()$season_current) {
    .season <- season
    data %>%
      .recode_tm_cols_espn(...) %>%
      # .add_timeperiod_cols_nfl(...) %>%
      .arrange_gm_nfl(..., season = .season)
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

# .get_grid_url_nfl_espn <-
#   function(season = as.integer(format(Sys.Date(, "%Y"))), ...) {
#     .get_grid_nfl_espn(season = season, ...) %>%
#       mutate(url = purrr::pmap_chr(
#         list(season, seasontype, wk),
#         ~ espn2::make_url_scores_nfl(
#           year = ..1,
#           seasontype = ..2,
#           week = ..3
#         )
#       ))
#
#   }

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
  function(data, ...) {
    data %>%
      mutate(data = purrr::map2(data, season, ~.finalize_xxx_nfl_espn(data = .x, season = .y))) %>%
      unnest()
  }

do_get_scores_nfl_espn <-
  function(wk, season = config::get()$season_current, ..., seasontype = 2L) {
    data_raw <-
      .preprocess_do_get_xxx_nfl_espn(
        wk = wk,
        season = season,
        seasontype = seasontype,
        ...
      )

    data_raw %>%
      mutate(data = purrr::map(data, ~.filter_scores_nfl_espn(data = .x, ...))) %>%
      mutate(data = purrr::map(data, ~.clean_scores_nfl_espn(data = .x, ...))) %>%
      .postprocess_do_get_xxx_nfl_espn(...)
  }

do_get_scores_season_nfl_espn <-
  function(season = config::get()$season_current, ..., seasontype = 1L:3L, wk = 1L:17L) {
    do_get_scores_nfl_espn(
      season = season,
      seasontype = seasontype,
      wk = wk,
      ...
    )
    
  }

do_get_odds_nfl_espn <-
  function(wk, season = config::get()$season_current, ..., seasontype = 2L) {
    data_raw <-
      .preprocess_do_get_xxx_nfl_espn(
        wk = wk,
        season = season,
        seasontype = seasontype,
        ...
      )

    data_raw %>%
      .filter_odds_nfl_espn(...) %>%
      .clean_odds_nfl_espn(...) %>%
      .postprocess_do_get_xxx_nfl_espn(...)
  }
