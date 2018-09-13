
COL_ESPN <- "name"
RGX_SPLIT_ESPN <- "\\."
.get_cols_max_at <-
  function(data, col, rgx_split) {
    if(missing(col)) {
      col <- COL_ESPN
    }
    if(missing(rgx_split)) {
      rgx_split <- RGX_SPLIT_ESPN
    }
    col_sym <- sym(col)
    data %>%
      pull(!!col_sym) %>% 
      str_split(rgx_split) %>% 
      map_dbl(~length(.)) %>% 
      max()
  }

.separate_cols_at <-
  function(data, col, rgx_split) {
    if(missing(col)) {
      col <- COL_ESPN
    }
    if(missing(rgx_split)) {
      rgx_split <- RGX_SPLIT_ESPN
    }
    col_sym <- sym(col)
    n_cols_max <-
      .get_cols_max_at(data = data, col = col)
    nms_sep <-
      paste0(col, seq(1, n_cols_max, by = 1))
    data %>%
      separate(!!col_sym, into = nms_sep, sep = rgx_split)    
  }

.convert_list_to_tbl <-
  function(x) {
    tibble::enframe(unlist(x))
  }

.convert_list_to_tbl_cleanly_at <-
  function(x, col, rgx_split) {
    if(missing(col)) {
      col <- COL_ESPN
    }
    if(missing(rgx_split)) {
      rgx_split <- RGX_SPLIT_ESPN
    }
    data <- tibble::enframe(unlist(x))
    .separate_cols_at(data = data, col = col, rgx_split = rgx_split)
  }


.rename_tm_cols <-
  function(data) {
    config <- config::get()
    nfl_tm <-
      config$path_nfl_tm %>% 
      readr::read_csv()
    
    data %>%
      inner_join(nfl_tm, by = c("tm_home" = "tm_espn")) %>% 
      mutate(tm_home = tm) %>% 
      select(-tm) %>% 
      inner_join(nfl_tm, by = c("tm_away" = "tm_espn")) %>% 
      mutate(tm_away = tm) %>% 
      select(-tm)
  }


.add_wk_col_at <-
  function(data, val, col = "wk") {
    col_sym <- sym(col)
    data %>%
      mutate(!!col := as.integer(val))
  }

.add_season_col_at <-
  function(data, val, col = "season") {
    col_sym <- sym(col)
    data %>%
      mutate(!!col := as.integer(val))
  }

.arrange_gm <-
  function(data) {
    config <- config::get()
    nfl_game_result <-
      config$path_nfl_game_result %>% 
      readr::read_csv() %>% 
      mutate(rn = row_number())
    
    data %>%
      inner_join(nfl_game_result) %>% 
      arrange(rn) %>% 
      select(-rn)
      
  }

