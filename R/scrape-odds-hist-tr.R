
url <- "https://www.teamrankings.com/nfl/trends/win_trends/?sc=is_after_win&range=yearly_2017"
url <- "https://www.teamrankings.com/nfl/trends/win_trends/?sc=is_after_win&range=yearly_since_2003"


.get_valid_trends <- function() {
  c("win_trends", "ats_trends", "ou_trends")
}
.get_valid_scs <- function() {
  c(
    "is_after_bye",
    "is_after_win",
    "is_after_loss",
    "is_home",
    "is_away",
    "is_fav",
    "is_dog",
    "is_home_fav",
    "is_home_dog",
    "is_away_fav",
    "is_away_dog",
    "is_conference",
    "non_conference",
    "is_division",
    "non_division",
    "is_playoff"
  )
}
.get_valid_ranges <- function() {
  c(sprintf("yearly_%s", seq(2003, 2018, 1)),
    "yearly_since_2003",
    "yearly_all")
}

trends <- .get_valid_trends()
scs <- .get_valid_scs()
ranges <- c(sprintf("yearly_%s", seq(2016, 2017, 1)))

fmt_url <- "https://www.teamrankings.com/nfl/trends/%s/?sc=%s&range=%s"
url_grid <-
  expand.grid(
    trend = trends,
    sc = scs,
    range = ranges,
    stringsAsFactors = FALSE
  ) %>%
  as_tibble() %>% 
  mutate(url = sprintf(fmt_url, trend, sc, range))
url_grid

.f_scrape_trend_tr <- function(url) {
  url %>%
    xml2::read_html() %>% 
    rvest::html_node("table") %>% 
    rvest::html_table(header = TRUE)
}

# col_names_orig <- names(url_grid)
# col_names_new <-
#   c(
#     "tm",
#     "win_wlt",
#     "win_w_pct",
#     "win_mov",
#     "win_ats",
#     "ats_wlt",
#     "ats_w_pct",
#     "ou_over_wlt",
#     "ou_over_pct",
#     "ou_under_pct",
#     "ou_total"
#   )

.f_rename_trend_tr_data <-
  function(data, trend = .get_valid_trends()) {
    col_names0 <- c("tm", "wlt")
    col_names1 <- c(col_names0, "w_pct", "mov", "ats")
    col_names2 <- c(col_names0, "over_pct", "under_pct", "total")
    # trend <- match.arg(trend)
    trend <- str_remove_all(trend, "_trends$")
    col_names <-
      switch(
        trend,
        win = col_names1,
        ats = col_names1,
        ou = col_names2
      )
    data %>%
      purrr::set_names(col_names)
  }

trends_tr_hist_raw_grid <-
  trends_tr_hist_url_grid %>%
  mutate(data = purrr::map(url, .f_scrape_trend_tr))
data_raw_grid %>% unnest()

trends_tr_hist <-
  trends_tr_hist_grid %>%
  mutate_at(vars(trend), funs(str_remove(., "_trends$"))) %>% 
  mutate(data = purrr::map2(data, trend, .f_rename_trend_tr_data)) %>% 
  unnest() %>% 
  mutate(season = str_remove(range, "^yearly_") %>% as.integer()) %>% 
  select(-range, -url) %>% 
  select(trend, sc, season, everything()) %>% 
  separate(wlt, into = c("w", "l", "t"), sep = "-") %>% 
  select(-matches("_pct$")) %>% 
  mutate_at(vars(w, l, t), funs(as.integer)) %>% 
  mutate_at(vars(matches("mov|ats|total")), funs(as.numeric)) %>% 
  .fix_tm_col_nfl_tr_at(col = "tm") %>% 
  arrange(trend, sc, season, tm)
trends_tr_hist

path_trends_tr_hist <- "data/trends_tr_hist.csv"
trends_tr_hist %>% write_csv(path_trends_tr_hist)

summ_odds_tr_hist <-
  data %>%
  group_by(trend, sc, season) %>% 
  mutate(n = 1) %>% 
  summarise_at(vars(w, l, t, mov, ats, total, n), funs(sum(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(w_pct = w / (w + l + t)) %>% 
  mutate_at(vars(mov, ats, total), funs(. / n)) %>% 
  select(-n)
summ_odds_tr_hist
