
yr1 <- 2018L
yr2 <- 2018L
n_gm_per_reg <- 256L
n_gm_per_post <- 11L
n_gm_per_season <- n_gm_per_reg + n_gm_per_post
rows_per_pg <- 100L
url_fmt <-
  paste0(
    "https://www.pro-football-reference.com/play-index/tgl_finder.cgi?request=1&match=game&year_min=",
    yr1,
    "&year_max=",
    yr2,
    "&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&temperature_gtlt=lt&c1stat=vegas_line&c1comp=gt&c5val=1.0&order_by=game_date&order_by_asc=Y&offset="
  )
url <- sprintf("%s%.0f", url_fmt, 0 * rows_per_pg)
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
