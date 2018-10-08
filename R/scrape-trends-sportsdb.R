
.URL_TRENDS_NFL_SPORTSDB <- "http://sportsdatabase.com/nfl/trends"


# url <- .URL_TRENDS_NFL_SPORTSDB
url <- "http://sportsdatabase.com/nfl/trends?output=matchups&active_week=6&owner_pats_request=SDB%3A%3A.*_.*"
html <- url %>% xml2::read_html()
html
.COLS_TRENDS_NFL_SPORTSBD <- c("tm", "ats", "ou", "su")
html_table <-
  html %>%
  # rvest::html_table() %>% 
  rvest::html_nodes("table") %>% 
  pluck(4) %>% 
  rvest::html_table() %>% 
  set_names(.COLS_TRENDS_NFL_SPORTSBD) %>% 
  as_tibble()
html_table

html_clean0 <-
  html_table %>%
  filter(tm != "") %>%
  mutate_at(vars(tm), funs(str_replace_all(., "\\s+\\:.*$", ""))) %>%
  mutate(rn = row_number()) %>%
  mutate(grp = ceiling(rn / 2)) %>%
  mutate_at(
    vars(matches("ats|ou|su")),
    funs(
      str_replace_all(., "\\s+[Tt]rends?", "") %>%
        str_replace_all("No", "0")
    )
  ) %>%
  group_by(grp) %>%
  mutate(rn_grp = row_number()) %>%
  ungroup() %>%
  gather(key, value, tm:su) %>%
  select(-rn) %>%
  mutate_at(vars(rn_grp), funs(if_else(. == 1L, "away", "home"))) %>%
  unite(key, key, rn_grp, sep = "_") %>%
  spread(key, value) %>%
  select(
    grp,
    tm_home,
    tm_away,
    ats_home,
    ats_away,
    ou_home,
    ou_away,
    su_home,
    su_away
  )

html_clean1 <-
  html_clean0 %>% 
  mutate_at(
    vars(matches("ats|ou|su")),
    funs(
      record = str_replace_all(., "(^.*\\s+\\()(.*)\\)(\\s?$)", "\\2")
    )
  ) %>% 
  mutate_at(
    vars(matches("(ats|ou|su)_(home|away)$")),
    funs(str_replace_all(., "\\s+\\(.*", ""))
  ) %>% 
  rename_at(vars(matches("(ats|ou|su)_(home|away)$")), funs(paste0("n_", .))) %>% 
  mutate_at(
    vars(matches("record$")),
    funs(
      w = str_replace_all(., "\\s+\\-.*", ""), 
         l = str_replace_all(., ".*\\-\\s+", "")
    )
  )
html_clean2 <-
  html_clean1 %>%
  select(-matches("^n_|record$")) %>% 
  rename_at(vars(matches("record_")), funs(str_replace_all(., "record_", ""))) %>% 
  # gather() first?
  rename_at(vars(matches("_w$")), funs(paste0("w_", str_replace_all(., "_w$", "")))) %>% 
  rename_at(vars(matches("_l$")), funs(paste0("l_", str_replace_all(., "_l$", ""))))
html_clean2
