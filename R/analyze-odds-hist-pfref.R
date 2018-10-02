
odds_hist_pfref <-
  config$path_odds_hist_pfref %>% 
  teproj::import_path_cleanly()



# prepare ----
odds_hist_pfref_aug <-
  odds_hist_pfref %>% 
  mutate_at(vars(spread_home, total), funs(tier = ggplot2::cut_number(., n = 8)))

odds_hist_pfref_summ <-
  odds_hist_pfref_aug %>% 
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
# debug(tetidy::summarise_stats_at)
# undebug(tetidy::summarise_stats_at)



# analyze ----
odds_hist_pfref_summ %>% 
  group_by(spread_home_tier, tm_fav_win) %>% 
  # summarise_at(vars(pts_diff_spread), funs(n = n(), mean(., na.rm = TRUE), sd(., na.rm = TRUE)))
  tetidy::summarise_stats(pts_diff_spread)

odds_hist_pfref_summ %>% count(tm_winner_spread) %>% arrange(n)
odds_hist_pfref_summ %>% count(spread_home_tier)
odds_hist_pfref_summ %>% count(fav_win)

