

.URL_QUERY_NFL_FORMAT_SPORTSDB <-
  "http://sportsdatabase.com/nfl/query?output=default&sdql=%s&submit=++S+D+Q+L+%%21++"

.encode_url_query_sportsdb <-
  function(x) {
    x %>% 
      str_replace_all("\\s", "+") %>% 
      str_replace_all("\\<", "%3C") %>%  
      str_replace_all("\\>", "%3E") %>% 
      str_replace_all("\\=", "\\%3D") %>% 
      str_replace_all("\\:", "%3A")
  }
.get_encoded_url_query_nfl_sportsdb <-
  function(sdql, ..., format = .URL_QUERY_NFL_FORMAT_SPORTSDB) {
    sprintf(format, .encode_url_query_sportsdb(sdql))
  }

.add_and_clause <-
  function(x, chr, value, value_buffer) {
    if(is.null(value)) {
      return(x)
    }
    if(x != "") {
      x <- paste0(x, " and ")
    }
    sprintf("%s%s >= %.1f and %s <= %.1f", x, chr, value - value_buffer, chr, value + value_buffer)
  }

.create_query_nfl_sportsdb <-
  function(line = NULL,
           total = NULL,
           pts_diff_lag1_home = NULL,
           pts_diff_lag1_away = NULL,
           line_diff_lag1_home = NULL,
           line_diff_lag1_away = NULL,
           total_diff_lag1_home = NULL,
           total_diff_lag1_away = NULL,
           ...,
           .line_buffer = ifelse(abs(line) %in% c(3, 7, 10), 0.5, 1),
           .total_buffer = 2) {
    res <- ""
    res <- 
      res %>% 
      .add_and_clause("line", line, .line_buffer) %>% 
      .add_and_clause("total", total, .total_buffer)
    res
  }

.do_get_url_query_nfl_sportsdb <-
  function(...) {
    query <- .create_query_nfl_sportsdb(...)
    .get_encoded_url_query_nfl_sportsdb(query, ...)
  }

sdql <- "line >= -7.5 and line <= -6.5 and total >= 50 and total <= 54 and p:ats margin >= 0 and P:ats margin <= 0"
sdql <- "line >= -3.5 and line >= -2.5 and total >= 50 and total <= 54 and p:ats margin >= 6 and P:ats margin <= -6"
.create_query_nfl_sportsdb(line = -3, total = 50)
.create_query_nfl_sportsdb(line = 3, total = 42)
url <- .do_get_url_query_nfl_sportsdb(line = -3, total = 50)

url <- sdql %>%.get_url_query_nfl_sportsdb()
url
html <- url %>% xml2::read_html()
odds_records_raw <-
  html %>%
  rvest::html_nodes("table") %>% 
  pluck(4) %>% 
  rvest::html_table() %>% 
  as_tibble() %>% 
  select(c(1:2))
odds_records_raw

odds_records_clean <-
  odds_records_raw %>%
  set_names(c("metric", "record")) %>% 
  mutate_at(vars(metric), funs(str_replace_all(., "[/:]", "") %>% tolower())) %>% 
  mutate_at(vars(record), funs(str_replace_all(., "\\s\\(.*$", ""))) %>% 
  separate(record, into = c("w", "l", "t")) %>% 
  select(-t) %>% 
  mutate_at(vars(matches("^(w|l)$")), funs(as.integer)) %>% 
  mutate(n = w + l) %>% 
  mutate(w_frac = w / n) %>% 
  select(metric, n, w_frac)
odds_records_clean
# node 4 <- overall stats
# node 5 <- average game box scores
# node 6 <- all records
URLencode("+")

html_table
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
