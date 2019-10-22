
if(!interactive()) {
  dir_wd <- 'C:/Users/aelhabr/Documents/projects/'
  # dir_wd <- 'O:/_other/projects/'
  prj <- 'sports-predict'
  wd <- file.path(dir_wd, prj)
  setwd(wd)
  invisible(source('.Rprofile'))
}

if(!interactive()) {
  msg <- sprintf('Started script at %s.', Sys.time())
  message(msg)
}

# undebug(do_get_odds_nfl_tr)
# undebug(.request_odds_tr)
odds_nfl_tr <- do_get_odds_nfl_tr()
success <- insert_into_db_odds_tr(data = odds_nfl_tr)
odds_nba_tr <- do_get_odds_nba_tr()
success <- insert_into_db_odds_tr(data = odds_nba_tr)

if(!interactive()) {
  msg <- sprintf('Finished script at %s.', Sys.time())
  message(msg)
}
