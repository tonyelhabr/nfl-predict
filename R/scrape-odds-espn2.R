
library("tidyverse")
lg <- "nfl"
wk <- 1
stype <- 2
yr <- 2018

scores <- espn2::get_scores_nfl(seasontype = stype, year = yr, week = wk)
scores
scores %>% readr::write_csv("temp.csv")

scores_min <- espn2::get_scores_nfl(seasontype = stype, year = yr, week = wk, as = "minimal")
scores_min
scores_min %>%
  filter(str_detect(name, "odd"))

n_cols_max <-
  scores_min %>%
  pull(name) %>% 
  str_split("\\.") %>% 
  map_dbl(~length(.)) %>% 
  max()
n_cols_max

nms_sep <-
  paste0("name", seq(1, n_cols_max, by = 1))
scores_min_sep <-
  scores_min %>%
  separate(name, into = nms_sep, sep = "\\.")

# competitions <-
#   scores_min_sep %>% 
#   filter(name1 == "events", name2 == "competitions")
odds_raw %>% tetidy::pull_distinctly(name3)
odds_raw <-
  scores_min_sep %>%
  filter((name1 == "events" &
            name2 %in% c("shortName")) |
           (
             name1 == "events" &
               name2 == "competitions" &
               name3 == "odds" & 
               name4 %in% c("details", "overUnder")
           )
  )
odds_raw
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
  select(tm_home, tm_away, tm_home_line, total)
odds
