
library("tidyverse")
paths_funcs <-
  list.files(
    path = "R",
    pattern = "func",
    recursive = FALSE,
    full.names = TRUE
  )
invisible(sapply(paths_funcs, source))
path_scores <- "scores-tmp.csv"
path_odds <- "nfl_odds.csv"

lg <- "nfl"
wk <- 2
stype <- 2
yr <- 2018

# scores <- espn2::get_scores_nfl(seasontype = stype, year = yr, week = wk)
# scores

# scores ----
scores_min_lag1 <-
  espn2::get_scores_nfl(seasontype = stype, year = yr, week = wk - 1, as = "minimal")
scores_min_lag1

scores_min_sep_lag1 <-
  scores_min_lag1 %>% 
  .separate_cols_at(col = "name")
scores_min_sep_lag1

scores_lag1_raw <-
  scores_min_sep_lag1 %>%
  filter((name1 == "events" &
            name2 == "shortName") |
           (
             name1 == "events" &
             name2 == "competitions" &
             name3 == "competitors" &
             name4 == "score"
           )
  )
scores_lag1_raw


scores_lag1 <-
  scores_lag1_raw %>% 
  select(isscore = name4, value) %>% 
  mutate_at(vars(isscore), funs(if_else(is.na(.), FALSE, TRUE))) %>% 
  mutate(gm = if_else(!isscore, value, NA_character_)) %>% 
  fill(gm, .direction = "down") %>% 
  # filter(!is.na(odds_type)) %>% 
  filter(gm != value) %>% 
  group_by(gm) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  mutate(tm_dir = if_else(rn == 1, "pts_home", "pts_away")) %>% 
  select(gm, tm_dir, value) %>% 
  spread(tm_dir, value) %>% 
  separate(gm, into = c("tm_away", "tm_home"), sep = "(\\s+\\@\\s+)|(\\s+vs.*\\s+)") %>% 
  mutate_at(vars(matches("pts")), funs(as.integer)) %>% 
  .add_season_col_at(val = yr) %>% 
  .add_wk_col_at(val = wk - 1L) %>% 
  select(season, wk, tm_home, tm_away, pts_home, pts_away) %>% 
  .rename_tm_cols() %>% 
  .arrange_gm()
scores_lag1

scores_lag1 %>% write_csv(path_scores)

scores_min <-
  espn2::get_scores_nfl(seasontype = stype, year = yr, week = wk, as = "minimal")
scores_min

scores_min_sep <-
  scores_min %>% 
  .separate_cols_at(col = "name")
scores_min_sep

# odds ----
odds_raw <-
  scores_min_sep %>%
  filter((name1 == "events" &
            name2 == "shortName") |
           (
             name1 == "events" &
               name2 == "competitions" &
               name3 == "odds" & 
               name4 %in% c("details", "overUnder")
           )
  )
odds_raw
# odds_raw %>% tetidy::pull_distinctly(name3)

odds <-
  odds_raw %>% 
  select(isodds = name3, odds_type = name4, value) %>% 
  mutate_at(vars(isodds), funs(if_else(is.na(.), FALSE, TRUE))) %>% 
  mutate(gm = if_else(!isodds, value, NA_character_)) %>% 
  fill(gm, .direction = "down") %>% 
  # filter(!is.na(odds_type)) %>% 
  filter(gm != value) %>% 
  select(gm, odds_type, value) %>% 
  spread(odds_type, value) %>% 
  separate(gm, into = c("tm_away", "tm_home"), sep = "(\\s+\\@\\s+)|(\\s+vs.*\\s+)") %>% 
  separate(details, into = c("tm_favored", "line"), sep = "\\s+") %>%
  mutate_at(vars(line, overUnder), funs(as.numeric)) %>% 
  mutate(tm_home_line = if_else(tm_home == tm_favored, line, -line)) %>% 
  rename(total = overUnder) %>% 
  mutate_at(vars(total), funs(if_else(. <= 0, NA_real_, .))) %>% 
  .add_season_col_at(val = yr) %>% 
  .add_wk_col_at(al = wk) %>% 
  select(season, wk, tm_home, tm_away, tm_home_line, total) %>% 
  .rename_tm_cols()  %>% 
  .arrange_gm()
odds

# odds %>% write_csv(path_odds)
