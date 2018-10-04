
odds_hist_pfref <-
  config$path_odds_hist_pfref %>% 
  teproj::import_path_cleanly()



# prepare ----
.odds_hist_pfref_aug <-
  odds_hist_pfref %>% 
  mutate_at(vars(spread_home, total), funs(tier = ggplot2::cut_number(., n = 8)))

odds_hist_pfref_aug <-
  .odds_hist_pfref_aug %>% 
  mutate(
    pts_diff = abs(pts_home - pts_away), 
    pts_diff_spread = abs(pts_home - pts_away + spread_home)
  ) %>% 
  mutate(
    tm_home_win = 
      case_when(
        tm_home == tm_winner_straight ~ TRUE,
        tm_away == tm_winner_straight ~ FALSE,
        is.na(tm_winner_straight) ~ NA,
        TRUE ~ NA
      ),
    tm_fav_win = 
      case_when(
        spread_home < 0 & tm_home == tm_winner_spread ~ TRUE,
        spread_home > 0 & tm_away == tm_winner_spread ~ TRUE,
        is.na(tm_winner_spread) ~ NA,
        TRUE ~ FALSE
      )
  ) %>% 
  mutate_at(
    vars(pts_diff_spread), 
    funs(case_when(
      tm_fav_win == TRUE ~ .,
      tm_fav_win == FALSE ~ -.,
      TRUE ~ 0
    )
    )
  ) %>% 
  mutate_at(vars(pts_diff_spread), funs(as.integer))

# analyze ----
tms <-
  odds_hist_pfref_aug %>% 
  get_distinct_tms_at()

.summarise_f <-
  function(data, tm) {
    .tm <- tm
    data %>%
      filter(tm_home == .tm | tm_away == .tm) %>%
      mutate(is_tm_home = if_else(tm_home == tm, TRUE, FALSE)) %>%
      mutate(tm = if_else(is_tm_home, tm_home, tm_away)) %>%
      mutate(t_spread = if_else(is.na(tm_winner_spread), 1L, 0L),) %>%
      mutate(
        w_spread = if_else(tm == tm_winner_spread, 1L, 0L),
        l_spread = if_else(tm != tm_winner_spread &
                             !t_spread, 1L, 0L)
      ) %>%
      mutate(
        spread_tm = if_else(is_tm_home, spread_home, -spread_home),
        pts_diff_spread_tm = if_else(
          is_tm_home,
          pts_home - pts_away + spread_home,
          -(pts_home - pts_away + spread_home)
        )
      ) %>%
      # mutate_at(vars(spread_home, pts_diff_spread), funs(if_else(is_tm_home, ., -.))) %>%
      # rename(spread_tm = spread_home, pts_diff_spread_tm = pts_diff_spread) %>%
      mutate(
        pts_diff_spread_tm_w = if_else(w_spread == 1L, pts_diff_spread_tm, NA_real_),
        pts_diff_spread_tm_l = if_else(l_spread == 1L, pts_diff_spread_tm, NA_real_)
      ) %>%
      mutate(n = 1L) %>%
      group_by(season, tm) %>%
      summarise_at(
        vars(
          n,
          w_spread,
          l_spread,
          spread_tm,
          pts_diff_spread_tm,
          pts_diff_spread_tm_w,
          pts_diff_spread_tm_l
        ),
        funs(sum(., na.rm = TRUE))
      ) %>%
      ungroup()
  }

summ_odds_bytm_byseason <-
  tms %>%
  mutate(data = purrr::map(tm, ~.summarise_f(odds_hist_pfref_aug, tm = .x))) %>% 
  select(-tm) %>% 
  unnest(data)
summ_odds_bytm_byseason %>% 
  arrange(desc(season), spread_tm)
summ_odds_bytm_byseason %>% 
  arrange(desc(season), desc(pts_diff_spread_tm))
summ_odds_bytm_byseason %>% 
  arrange(desc(season), desc(w_spread))

odds_hist_pfref_aug %>% 
  group_by(season, tm_home) %>% 
  summarise_at(vars(spread_home, pts_diff_spread), funs(sum(.))) %>% 
  ungroup() %>% 
  arrange(season, tm_home)


odds_hist_pfref_aug %>% 
  group_by(spread_home_tier, tm_fav_win) %>% 
  # summarise_at(vars(pts_diff_spread), funs(n = n(), mean(., na.rm = TRUE), sd(., na.rm = TRUE)))
  tetidy::summarise_stats(pts_diff_spread)

odds_hist_pfref_aug %>% count(tm_winner_spread) %>% arrange(n)
odds_hist_pfref_aug %>% count(spread_home_tier)
odds_hist_pfref_aug %>% count(fav_win)

