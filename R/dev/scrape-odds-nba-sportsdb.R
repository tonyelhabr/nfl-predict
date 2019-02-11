
url <- paste0("http://sportsdatabase.com/nba/query?output=default&sdql=date%2Cteam%2Cline%2Ctotal%2C%40season%3D", i-1, "&submit=++S+D+Q+L+%21++")
url <-
  "http://sportsdatabase.com/nba/query?output=default&sdql=date%2C+t%3Ateam+as+tm_home%2C+o%3Ateam+as+tm_away%2C+t%3Apoints%2C+o%3Apoints%2C+line%2C+total+%40season+%3E%3D+2017&submit=++S+D+Q+L+%21++"

html <-
  url %>% readr::read_html()
