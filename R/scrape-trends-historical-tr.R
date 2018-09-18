
.get_valid_trends_tr <- function() {
  c("win", "ats", "ou")
}
.get_valid_scs_tr <- function() {
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
.get_valid_ranges_tr <- function() {
  c(sprintf("%s", seq(2003, 2018, 1)),
    "since_2003",
    "all")
}

trends <- .get_valid_trends_tr()
scs <- .get_valid_scs_tr()
ranges_valid <- .get_valid_ranges_tr()
ranges <- seq(2016, 2017, 1)
all(ranges %in% ranges_valid)

fmt_url <- "https://www.teamrankings.com/nfl/trends/%s_trends/?sc=%s&range=yearly_%s"
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

.f_rename_trend_tr_data <-
  function(data, trend = .get_valid_trends(), sep = "_") {
    
    # trend <- match.arg(trend)
    col_names0 <- c("tm")
    col_names_pre1 <- c("wlt", "wpct", "mov", "ats")
    col_names_pre2 <- c("wlt", "overpct", "underpct", "total")
    col_names1 <- c(col_names0, paste0(col_names_pre1, sep, trend))
    col_names2 <- c(col_names0, paste0(col_names_pre2, sep, trend))
    
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

data_raw_grid <-
  url_grid %>%
  mutate(data = purrr::map(url, .f_scrape_trend_tr))
data_raw_grid %>% unnest()

unlist(strsplit("abc", ""))

.separate_col_wlt_trend_tr_at <-
  function(data,
           trend,
           ...,
           sep = "_",
           .sep = "-",
           col = paste0("wlt", sep, trend),
           into = paste0(c("w", "l", "t"), sep, trend)) {
    col_sym <- sym(col)
    data %>%
      separate(!!col_sym, into = into, sep = .sep, ...)
  }

.add_col_wpct_trend_tr_at <-
  function(data,
           trend,
           ...,
           sep = "_",
           cols_wlt = paste0(c("w", "l", "t"), sep, trend),
           col_out = paste0("wpct", sep, trend)) {
    col_out_sym <- sym(col_out)
    col_w_sym <- sym(cols_wlt[1])
    col_l_sym <- sym(cols_wlt[2])
    col_t_sym <- sym(cols_wlt[3])
    data %>%
      mutate(!!col_out_sym := !!col_w_sym / (!!col_w_sym + !!col_l_sym))
  }

rgx_vars <- "_(win|ats|ou)$"
trends_tr_hist <-
  data_raw_grid %>%
  select(-url) %>% 
  mutate(season = str_remove(range, "^yearly_") %>% as.integer()) %>% 
  select(trend, sc, range, season, everything()) %>% 
  mutate(data = purrr::map2(data, trend, .f_rename_trend_tr_data)) %>% 
  unnest() %>% 
  .separate_col_wlt_trend_tr_at(trend = "win") %>% 
  .separate_col_wlt_trend_tr_at(trend = "ats") %>% 
  .separate_col_wlt_trend_tr_at(trend = "ou") %>% 
  select(-trend, -matches("pct|(ats_win)|(mov_ats)")) %>% 
  group_by(sc, range, season, tm) %>% 
  # NOTE: Could use a better way to "flatten" this data.
  summarise_at(vars(matches(rgx_vars)), funs(max(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate_at(vars(matches(rgx_vars)), funs(as.numeric)) %>% 
  mutate_at(vars(matches("^[wlt]_")), funs(as.integer)) %>% 
  .fix_tm_col_nfl_tr_at(col = "tm") %>% 
  arrange(sc, season, tm)
trends_tr_hist

path_trends_tr_hist <- "data/trends_tr_hist.csv"
trends_tr_hist %>% write_csv(path_trends_tr_hist)

trends_tr_hist %>% count(sc)

rgx_vars_signif <- "(mov|ats|total)_"
summ_trends_tr_hist_byyear <-
  trends_tr_hist %>% 
  filter(str_detect(sc, "conference")) %>% 
  group_by(sc, tm, season) %>% 
  mutate(n_gm = sum(w_win + l_win + t_win)) %>% 
  ungroup() %>% 
  group_by(tm, season) %>% 
  summarise_at(vars(matches(rgx_vars_signif)), funs(sum(n_gm * .) / sum(n_gm))) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  mutate_at(vars(matches(rgx_vars_signif)), funs(rnk = row_number(desc(.)))) %>% 
  ungroup()
summ_trends_tr_hist_byyear

n_tier <- 8
rgx_vars_signif_rnk <- paste0(rgx_vars_signif, ".*rnk$")

summ_trends_tr_hist_byyear_rnks <-
  summ_trends_tr_hist_byyear %>% 
  mutate_at(
    vars(matches(rgx_vars_signif_rnk)), 
    funs(tier = ggplot2::cut_interval(., length = n_tier))
  ) %>% 
  rename_at(vars(matches("_rnk_tier")), funs(str_replace(., "_rnk_tier", "_tier")))

rgx_vars_signif_tier <- paste0(rgx_vars_signif, ".*tier$")
summ_trends_tr_hist_byyear_rnks %>% 
  select(matches(rgx_vars_signif_rnk)) %>% 
  corrr::correlate()

summ_trends_tr_hist_byyear_rnks %>% 
  select(matches(rgx_vars_signif_tier)) %>% 
  mutate_all(is.factor, as.integer) %>% 
  corrr::correlate()

summ_trends_tr_hist_byyear_rnks %>% 
  select(season, tm, matches(rgx_vars_signif_tier)) %>% 
  mutate_if(is.factor, as.integer) %>% 
  filter_at(vars(matches(rgx_vars_signif_tier)), all_vars(. == 4L))

summ_trends_tr_hist_bysc <-
  trends_tr_hist %>%
  group_by(sc) %>% 
  mutate(n = 1) %>% 
  summarise_at(vars(matches(rgx_vars), n), funs(sum(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate_at(vars(matches(rgx_vars_signif)), funs(. / n)) %>% 
  .add_col_wpct_trend_tr_at(trend = "win") %>% 
  .add_col_wpct_trend_tr_at(trend = "ats") %>% 
  .add_col_wpct_trend_tr_at(trend = "ou") %>% 
  select(-n)
summ_trends_tr_hist_bysc

