
.extract_tr_nfl_fl <-
  function(html) {
    html %>% 
      rvest::html_nodes("table tr") %>% 
      rvest::html_text() %>% 
      as.list() %>% 
      str_subset("0 ET[\r\n]") %>% 
      str_subset("(?!(reference).).*$") %>% 
      str_trim()
  }

.clean_tr_nfl_fl <-
  function(x) {
    x %>% 
      str_replace_all("\n", " ") %>% 
      str_replace_all("\\s+", " ")
  }


..filter_bylength <-
  function(x, max = 80) {
    ifelse(nchar(x) > max, NA, x)
    # NOTE: This doesn't work as intended.
  }

.filter_tr_nfl_fl <-
  function(x) {
    res <- ..filter_bylength(x)
    res[!is.na(res)]
  }


.clean_nfl_fl <-
  function(data, ...) {
    data %>%
      as_tibble() %>%
      separate(value, into = c("time", "gm"), sep = "\\s+ET\\s+") %>%
      separate(time,  into = c("date", "time"), sep = "\\s+") %>% 
      mutate(tm_home_isfav = if_else(str_detect(gm, "^At"), TRUE, FALSE)) %>%
      mutate(tm_home = str_replace(gm, "(^.*At\\s+)([A-Za-z\\s]+)(\\s[\\-?0-9].*$)", "\\2")) %>%
      mutate(tm_away = 
               str_extract_all(gm, "([A-Za-z\\s]+)") %>% 
               purrr::map2_chr(ifelse(tm_home_isfav, 2, 1), ~.[[.y]])) %>% 
      mutate(spread = str_replace(gm, "(^.*[a-z]\\s+)(\\-?[0-9.]+)(\\s+[A-Z].*$)", "\\2")) %>%
      mutate(total = str_replace_all(gm, "(^.*)(\\s[0-9.]+)$", "\\2")) %>% 
      mutate_if(is.character, str_trim) %>% 
      mutate_at(vars(spread, total), funs(as.numeric)) %>% 
      mutate_at(vars(spread), funs(if_else(tm_home_isfav, ., -.))) %>% 
      mutate(date = sprintf("%s/%s", date, format(Sys.Date(), "%Y")) %>% lubridate::mdy()) %>% 
      mutate(time = lubridate::ymd_hm(paste0(date, " ", time))) %>% 
      select(-gm, -tm_home_isfav)
  }

.recode_tm_cols_fl <-
  function(data, ...) {
    .recode_tm_cols_at(
      data = data,
      col = "tm_name_fl"
    )
  }
