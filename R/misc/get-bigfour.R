
# bigfour reference: https://github.com/bigfour/competitiveness/blob/master/data/bigfour_public.rda.
.URL_BIGFOUR_PUBLIC <- "https://github.com/bigfour/competitiveness/blob/master/data/bigfour_public.rda"

download.file(url, destfile = "", quiet = TRUE)
bigfour_public <-
  file.path("data-raw", "bigfour_public.rda") %>% 
  teproj::import_path_cleanly()
bigfour_public

bigfour_nfl <-
  bigfour_public %>%
  filter(sport == "nfl") %>%
  mutate(date = format(game_date, "%Y%m%d") %>% lubridate::ymd()) %>%
  mutate(season = if_else(
    date %>% lubridate::month() <= 2,
    date %>% lubridate::year() - 1,
    date %>% lubridate::year()
  )) %>%
  mutate_at(vars(season), funs(as.integer)) %>% 
  select(date, season, everything()) %>%
  select(-game_date, -sport) %>%
  rename(
    tm_away_name_full = visitor_team,
    tm_home_name_full = home_team,
    pts_away = visitor_score,
    pts_home = home_score,
    prob_home = p_home
  ) %>%
  arrange(season, date, tm_home_name_full, tm_away_name_full)
bigfour_nfl

bigfour_nfl_tm <-
  bigfour_nfl %>%
  left_join(nfl_tm, by = c("tm_away_name_full" = "tm_name_full")) %>%
  rename(tm_away = tm) %>%
  left_join(nfl_tm, by = c("tm_home_name_full" = "tm_name_full")) %>%
  rename(tm_home = tm) %>%
  select(-ends_with("name_full")) %>%
  select(date, season, tm_home, tm_away, pts_home, pts_away, prob_home)
bigfour_nfl_tm
# bigfour_nfl_tm %>% filter(is.na(tm_home) | is.na(tm_away))
# bigfour_nfl_tm %>%
#   mutate(rnk_fav = row_number(desc(prob_home))) %>%
#   arrange(rnk_fav) %>%
#   slice(1:10)
