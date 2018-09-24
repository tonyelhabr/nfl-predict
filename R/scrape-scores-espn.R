
# TODO: Programmatically identify week?
.wk <- 4

scores_lag1 <- do_get_scores_nfl_espn(wk = .wk - 1)
scores_lag1
scores_lag1 %>% teproj::export_path(config$path_scores_temp)

# odds <- do_get_odds_nfl_espn(wk = .wk)
# odds
# odds %>% write_csv(config$path_odds_temp)