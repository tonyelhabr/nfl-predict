
# .extract_tr_nfl_fl <-
#   function(html) {
#     html %>% 
#       rvest::html_nodes("table tr") %>% 
#       rvest::html_text() %>% 
#       as.list() %>% 
#       str_subset("[05] ET[\r\n]") %>% 
#       str_subset("(?!(reference).).*$") %>% 
#       str_trim()
#   }
# 
# .clean_tr_nfl_fl <-
#   function(x) {
#     x %>% 
#       str_replace_all("\n", " ") %>% 
#       str_replace_all("\\s+", " ")
#   }
# 
# ..filter_bylength <-
#   function(x, max = 80) {
#     ifelse(nchar(x) > max, NA, x)
#     # NOTE: This doesn't work as intended.
#   }
# 
# .filter_tr_nfl_fl <-
#   function(x, ...) {
#     res <- ..filter_bylength(x, ...)
#     res[!is.na(res)]
#   }
# 
# .filter_xxx_nfl_fl <-
#   function(data, ...) {
#     data %>% 
#       .extract_tr_nfl_fl() %>% 
#       .clean_tr_nfl_fl() %>% 
#       .filter_tr_nfl_fl() %>% 
#       as_tibble()
#   }
# 
# .clean_xxx_nfl_fl <-
#   function(data, ...) {
#     data %>%
#       separate(value, into = c("time", "gm"), sep = "\\s+ET\\s+") %>%
#       separate(time,  into = c("date", "time"), sep = "\\s+") %>% 
#       mutate_at(vars(gm), funs(str_replace(., "\\sPK\\s", " -0.0 "))) %>% 
#       mutate(tm_home_isfav = if_else(str_detect(gm, "^At"), TRUE, FALSE)) %>%
#       mutate(tm_home = str_replace(gm, "(^.*At\\s+)([A-Za-z\\s]+)(\\s[\\-?0-9].*$)", "\\2")) %>%
#       mutate(tm_away = 
#                str_extract_all(gm, "([A-Za-z\\s]+)") %>% 
#                purrr::map2_chr(ifelse(tm_home_isfav, 2, 1), ~.[[.y]])) %>% 
#       mutate(spread = str_replace(gm, "(^.*[a-z]\\s+)(\\-?[0-9.]+)(\\s+[A-Z].*$)", "\\2")) %>%
#       mutate(total = str_replace_all(gm, "(^.*)(\\s[0-9.]+)$", "\\2")) %>% 
#       mutate_if(is.character, str_trim) %>% 
#       mutate_at(vars(spread, total), funs(as.numeric)) %>% 
#       mutate_at(vars(spread), funs(if_else(tm_home_isfav, ., -.))) %>% 
#       mutate(date = sprintf("%s/%s", date, format(Sys.Date(), "%Y")) %>% lubridate::mdy()) %>% 
#       mutate(time = lubridate::ymd_hm(paste0(date, " ", time))) 
#   }
# 
# 
# .clean_odds_nfl_fl <-
#   function(data, ...) {
#     # fl_raw %>%
#     data %>% 
#       mutate(moneylines = str_replace(value, "(^.*)([+-]\\$.*[+-]\\$.*$)", "\\2")) %>% 
#       separate(moneylines, into = c("moneyline1", "moneyline2"), sep = "\\s") %>% 
#       mutate_at(vars(matches("moneyline")), funs(str_replace(., "\\$", "") %>% as.integer())) %>% 
#       mutate_at(vars(value), funs(str_remove(., "\\s[+-]\\$[0-9]+\\s[+-].*$"))) %>% 
#       .clean_xxx_nfl_fl() %>% 
#       mutate(moneyline_home = if_else(str_detect(gm, "^At"), moneyline1, moneyline2), 
#              moneyline_away = if_else(str_detect(gm, "^At"), moneyline2, moneyline1)) %>% 
#       select(-matches("moneyline[12]")) %>% 
#       select(-gm, -tm_home_isfav)
#       
#   }