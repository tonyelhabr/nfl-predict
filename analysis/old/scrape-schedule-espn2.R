
lg <- "nfl"
wk <- NULL
stype <- 2
yr <- 2018
# url <- espn2::make_url_scores_nfl(seasontype = stype, year = yr, week = NULL, limit = 500)
# resp <- espn2:request_scores_nfl(url = url)
# scores <- espn2:parse_scores_nfl(resp = resp)
scores <- espn2::get_scores_nfl(seasontype = stype, year = yr, week = NULL, limit = 500)
scores

# .rename_espn2_cols <-
#   function(x) {
#     x %>%
#       snakecase::to_snake_case() %>%
#       str_replace_all("team", "tm")
#   }

sched_espn2 <-
  scores %>%
  filter(teamScoreHome == 0, teamScoreAway == 0) %>%
  mutate_at(vars(matches("Score")), funs(if_else(. == 0L, NA_integer_, .))) %>%
  # rename_all(funs(.rename_espn2_cols)) %>%
  select(
    date = eventDate,
    location = venueFullName,
    tm_home = teamAbbreviationHome,
    tm_home_name = teamNameHome,
    tm_away = teamAbbreviationAway,
    tm_away_name = teamNameAway
  )
sched_espn2
path_sched_espn2 <- file.path("data", "schedule-nfl-2018-espn2.csv")

sched_espn2 %>%
  teproj::export_path(path_sched_espn2)
  