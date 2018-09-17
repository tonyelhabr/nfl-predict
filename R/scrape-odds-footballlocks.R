
url <- "http://www.footballlocks.com/nfl_lines.shtml"
# url <- "http://www.footballlocks.com/nfl_lines_week_1.shtml"
# url <- "http://www.footballlocks.com/nfl_odds_week_2.shtml#Closing%20NFL%20Odds%20Week%202,%202017"

html <- url %>% xml2::read_html()

.extract_tr_fl <-
  function(html) {
    html %>% 
      rvest::html_nodes("table tr") %>% 
      rvest::html_text() %>% 
      as.list() %>% 
      str_subset("0 ET\n") %>% 
      str_subset("(?!(reference).).*$") %>% 
      str_trim()
  }

.clean_tr_fl <-
  function(x) {
    x %>% 
      str_replace_all("\n", " ") %>% 
      str_replace_all("\\s+", " ")
  }


.filter_bylength <-
  function(x, max = 80) {
    ifelse(nchar(x) > max, NA, x)
    # NOTE: This doesn't work as intended.
  }

.filter_tr_fl <-
  function(x) {
    res <- .filter_bylength(x)
    res[!is.na(res)]
  }

tr <-
  html %>%
  .extract_tr_fl() %>% 
  .clean_tr_fl() %>% 
  .filter_tr_fl()
tr
# tm_fl <-
#   nfl_tm %>% 
#   pull(tm_name_espn)
# rgx_tm_fl <-
#   paste0("(", paste0(tm_fl, collapse = ")|("), ")")

