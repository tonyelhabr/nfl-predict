
library("tidyverse")

url <- "http://www.footballlocks.com/nfl_lines.shtml"
# url <- "http://www.footballlocks.com/nfl_lines_week_1.shtml"

html <- url %>% xml2::read_html()
tr