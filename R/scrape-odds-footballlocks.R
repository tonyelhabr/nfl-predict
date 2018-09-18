
"http://www.footballlocks.com/nfl_lines.shtml"
"http://www.footballlocks.com/nfl_odds_week_1.shtml#Closing%20NFL%20Odds%20Week%201,%202006"
"http://www.footballlocks.com/nfl_odds_week_17.shtml#Closing%20NFL%20Odds%20Week%2017,%202017"
"http://www.footballlocks.com/nfl_odds_week_17.shtml#Closing%20NFL%20Odds%20Week%2017,%202016-2017"
"http://www.footballlocks.com/nfl_odds_week_17.shtml#Closing%20NFL%20Odds%20Week%2017,%202006"
"http://www.footballlocks.com/nfl_odds_wild_card_playoff_games.shtml#Closing%20NFL%20Odds%20Wild%20Card%20Playoff%20Games,%202007"
"http://www.footballlocks.com/nfl_odds_divisional_playoff_games.shtml#Closing%20NFL%20Odds%20Divisional%20Playoff%20Games,%202007"
"http://www.footballlocks.com/nfl_odds_conference_championship_playoff_games.shtml#Closing%20NFL%20Odds%20Conference%20Championship%20Playoff%20Games,%202007"
"http://www.footballlocks.com/nfl_odds_super_bowl.shtml#Closing%20NFL%20Odds%20Super%20Bowl%20XLI%20(41),%202007"
# url <- "http://www.footballlocks.com/nfl_odds_week_2.shtml#Closing%20NFL%20Odds%20Week%202,%202017"

fmt <- "http://www.footballlocks.com/nfl_odds_week_%s.shtml#Closing%sNFL%sOdds%sWeek%s17,%s2006"
sep <- "%20"
# sprintf(fmt_url, 1L, sep, sep, sep, sep, sep)
grid_url_plyff <-
  tribble(
    ~wk, ~url,
    18L, "http://www.footballlocks.com/nfl_odds_wild_card_playoff_games.shtml#Closing%20NFL%20Odds%20Wild%20Card%20Playoff%20Games,%202007",
    19L, "http://www.footballlocks.com/nfl_odds_divisional_playoff_games.shtml#Closing%20NFL%20Odds%20Divisional%20Playoff%20Games,%202007",
    20L, "http://www.footballlocks.com/nfl_odds_conference_championship_playoff_games.shtml#Closing%20NFL%20Odds%20Conference%20Championship%20Playoff%20Games,%202007",
    21L, "http://www.footballlocks.com/nfl_odds_super_bowl.shtml#Closing%20NFL%20Odds%20Super%20Bowl%20XLI%20(41),%202007"
  )
grid_url <-
  expand.grid(
    wk = seq(1L, 17L, 1L)
  ) %>% 
  as_tibble() %>% 
  mutate(url = sprintf(fmt, wk, sep, sep, sep, sep, sep)) %>% 
  bind_rows(grid_url_plyff) %>% 
  mutate(path_dl = paste0("data-raw/", str_replace_all(url, "(^.*com\\/)(.*)(\\.shtml.*$)", "\\2"), ".html"))
grid_url

grid_url %>%
  mutate(dl = 
           purrr::walk2(
             url,
             path_dl,
             ~download.file(
               url = .x,
               destfile = .y,
               quiet = TRUE
             )
           )
  ) %>% 
  mutate(dl = if_else(file.exists(path_dl), TRUE, FALSE))

url <- "http://www.footballlocks.com/nfl_lines.shtml"
path_dl <- "data-raw/nfl_lines.html"
download.file(
  url = url,
  destfile = path_dl,
  quiet = TRUE
)

html <- xml2::read_html(url)
html_dl <- xml2::read_html(path_dl)
html
html_dl
html %>% rvest::html_nodes("table tr td a") %>% rvest::html_text()
html_dl %>% rvest::html_nodes("table tr td a") %>% rvest::html_text()

fl_raw_dl <-
  html_dl %>%
  # .extract_tr_nfl_fl() %>% 
  rvest::html_nodes("table tr") %>% 
  rvest::html_text() %>% 
  as.list() %>% 
  str_subset("0 ET") %>% 
  str_subset("(?!(reference).).*$") %>% 
  str_trim() %>% 
  .clean_tr_nfl_fl() %>% 
  .filter_tr_nfl_fl()
  

fl_raw <-
  html %>%
  .extract_tr_nfl_fl() %>% 
  .clean_tr_nfl_fl() %>% 
  .filter_tr_nfl_fl()


fl_clean <-
  fl_raw %>%
  .clean_nfl_fl()

fl_lines <-
  fl_clean %>%
  .recode_tm_cols_fl() %>% 
  .add_timeperiod_cols_nfl() %>% 
  .arrange_gm()
fl_lines
fl_lines %>% write_csv(config$path_lines_temp)

