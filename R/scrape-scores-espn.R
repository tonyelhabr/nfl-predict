
# TODO: Programmatically identify week?
.wk <- 2

# scores <- espn2::get_scores_nfl(seasontype = stype, year = yr, week = wk)
# scores

scores_lag1 <-
  do_get_scores_nfl_espn(wk = .wk - 1)
scores_lag1

odds <-
  do_get_odds_nfl_espn(wk = .wk)
odds

# scores %>% write_csv(config$path_scores_temp)
# odds %>% write_csv(config$path_odds_temp)