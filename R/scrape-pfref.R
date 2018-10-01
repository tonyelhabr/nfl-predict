
season <- 2018L
n_gm_per_reg <- 256L
n_gm_per_post <- 11L
n_gm_per_season <- n_gm_per_reg + n_gm_per_post
rows_per_pg <- 100L
n_pg <- ceiling(n_gm_per_season / rows_per_pg)
n_pg
url_fmt <-
  paste0(
    "https://www.pro-football-reference.com/play-index/tgl_finder.cgi?request=1&match=game&year_min=%.0f&year_max=%.0f",
    "&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&temperature_gtlt=lt&c1stat=vegas_line&c1comp=gt&c5val=1.0",
    "&order_by=game_date&order_by_asc=Y&offset=%.0f"
  )

url <- sprintf(url_fmt, season, season, 0 * rows_per_pg)
html <-
  url %>%
  xml2::read_html()
html_table <-
  html %>% 
  rvest::html_nodes("table") %>% 
  pluck(1)
html_data <-
  html_table %>% 
  rvest::html_table(header = FALSE) 
header <-
  html_data %>% 
  slice(2)
col_names <- header %>% as.character()

data_raw <-
  html_data %>% 
  slice(3:n()) %>% 
  tibble::as_tibble() %>% 
  filter(X1 != col_names[1]) %>% 
  purrr::set_names(col_names)
data_raw

data_clean0 <-
  data_raw %>%
  janitor::clean_names() %>%
  mutate_at(
    vars(date),
    funs(lubridate::ymd(.))
  ) %>% 
  mutate_at(
    vars(time),
    funs(paste0(date, " ", .) %>% lubridate::ymd_hm())
  ) %>% 
  mutate(
    wday = date %>% lubridate::wday(label = TRUE, abbr = TRUE)
  ) %>% 
  mutate(
    tm_home = if_else(x == "@", opp, tm),
    tm_away = if_else(x == "@", tm, opp)
  ) %>%
  separate(result, into = c("wl", "score"), sep = "\\s") %>%
  separate(score, into = c("pts1", "pts2"), sep = "\\-") %>%
  mutate(
    pts_home = if_else(x == "@", pts2, pts1),
    pts_away = if_else(x == "@", pts1, pts2)
  ) %>%
  mutate(
    total = over_under,
    total_result = 
      case_when(
        ou_result == "over" ~ "O",
        ou_result == "under" ~ "U",
        TRUE ~ "E"
      )
  ) %>% 
  mutate_at(vars(year, week, matches("pts")), funs(as.integer)) %>% 
  mutate_at(vars(spread, total), funs(as.double))

data_clean <-
  data_clean0 %>% 
  mutate(
    spread_sign =
      case_when(
        x == "@" & vs_line == "not covered" & ((pts1 - pts2) > spread) ~ -spread,
        x == "@" & vs_line == "not covered" & ((pts1 - pts2) < spread) ~ -spread,
        x == "@" & vs_line == "covered" & ((pts1 - pts2) > spread) ~ spread,
        x == "@" & vs_line == "covered" & ((pts1 - pts2) < spread) ~ -spread,
        # x == "" & vs_line == "not covered" & ((pts1 - pts2) > spread) ~ spread,
        # x == "" & vs_line == "not covered" & ((pts1 - pts2) < spread) ~ -spread,
        # x == "" & vs_line == "covered" & ((pts1 - pts2) > spread) ~ -spread,
        # x == "" & vs_line == "covered" & ((pts1 - pts2) < spread) ~ spread,
        TRUE ~ NA_real_
      )
  ) %>%
  mutate_at(vars(spread_sign), funs(as.double)) %>% 
  select(
    season = year,
    wk = week,
    date,
    time,
    wday,
    tm_home,
    tm_away,
    pts_home,
    pts_away,
    spread,
    spread_sign,
    spread_result = vs_line,
    total,
    total_result
  )
data_clean

# new ----
.get_odds_pfref <-
  function(url, ..., col_names = NULL) {
    browser()
    html <-
      url %>%
      xml2::read_html()
    html_table <-
      html %>% 
      rvest::html_nodes("table") %>% 
      pluck(1)
    html_data <-
      html_table %>% 
      rvest::html_table(header = FALSE) 
    header <-
      html_data %>% 
      slice(2)
    if(is.null(col_names)) {
      col_names <- header %>% as.character()
    }
    
    data_raw <-
      html_data %>% 
      slice(3:n()) %>% 
      tibble::as_tibble() %>% 
      filter(X1 != col_names[1]) %>% 
      purrr::set_names(col_names)
  }

url_fmt <-
  paste0(
    "https://www.pro-football-reference.com/play-index/tgl_finder.cgi?request=1&match=game&year_min=%.0f&year_max=%.0f",
    "&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&temperature_gtlt=lt&c1stat=vegas_line&c1comp=gt&c5val=1.0",
    "&order_by=game_date&order_by_asc=Y&offset=%.0f"
  )
grid_url <-
  tibble(
    # season = rep(2017L, n_pg)
    season = rep(2017L, 1)
  ) %>% 
  group_by(season) %>% 
  mutate(n_pg = row_number()) %>% 
  ungroup() %>% 
  mutate(url = sprintf(url_fmt, season, season, (n_pg - 1) * rows_per_pg))
grid_url

data_pfref <-
  grid_url %>%
  mutate(data = purrr::map(url, ~.get_odds_pfref(.x))) %>% 
  select(-n_pg, -url) %>% 
  unnest()
data_pfref
