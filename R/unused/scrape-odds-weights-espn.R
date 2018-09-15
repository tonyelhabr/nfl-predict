
library("tidyverse")
url <- "http://fantasy.espncdn.com/nfl-pigskin-pickem/2017/1/en/api/matchups"

resp <- httr::GET(url)
resp
cont <- httr::content(resp)
probs_raw <-
  cont %>% 
  .convert_list_to_tbl()
probs_raw

probs_sep <-
  probs_raw %>% 
  .separate_cols_at(col = "name")
probs_sep

probs_raw <-
  probs_sep %>%
  filter((name3 == "p") | (name3 == "n_s" ))
probs_raw

# NOTE: It doesn't seem possible to get the line from this data. Also, it doesn't
# seem possible to go back in time without having an entry.