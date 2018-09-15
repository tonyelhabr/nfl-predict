
url <- "http://www.footballlocks.com/nfl_lines.shtml"
# url <- "http://www.footballlocks.com/nfl_lines_week_1.shtml"

html <- url %>% xml2::read_html()
tr <-
  html %>%
  rvest::html_nodes("table tr") %>% 
  rvest::html_text() %>% 
  as.list() %>% 
  str_subset("0 ET\n") %>% 
  # str_subset("(?!(CDATA).).*$") %>% 
  str_subset("(?!(reference).).*$") %>% 
  str_trim()
tr[[5]]
tr %>% as.list() %>% View()
t <- html %>% rvest::html_nodes("table") %>% pluck(10) %>% rvest::html_table()
tr
tr[20]
tr[30]
tr[35]
tr[40]
tr[50]
as.list(tr) -> z
