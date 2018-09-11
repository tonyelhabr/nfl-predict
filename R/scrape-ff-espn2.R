
yr <- 2017
lid <- 453218
ff_min <-
  espn2::get_ff(year = yr, leagueId = lid, as = "minimal")
ff_min


ff_min_sep <-
  ff_min %>%
  .separate_cols_at(col = "name")
ff_min_sep

matchups <-
  ff_min_sep %>%
  filter(name5 == "matchups")
matchups
tm_away_pts <-
  matchups %>%
  filter(name6 == "awayTeamScores")
tm_away_pts

matchups %>% 
  filter(name6 == "awayTeam") %>% 
  tetidy::pull_distinctly(name7)

matchups %>% 
  filter(name6 == "awayTeam") %>% 
  # filter(name7 %in% c("teamLocation", "teamNickname")) %>% 
  filter(name7 == "teamNickname") %>% 
  select(wk = name3, label = name7, value)
  # count(label, value, sort = T)
