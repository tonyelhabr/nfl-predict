

get_db_conn_odds_tr <-
  function(..., path = config$path_odds_tr) {
    get_db_conn(path)
  }
# Or...
# get_db_conn_odds_tr <-
#   purrr::partial(get_db_conn, config$path_odds_tr)

insert_into_db_odds_tr <- 
  function(..., conn = NULL) {
    if(is.null(conn)) {
      conn <- get_db_conn_odds_tr(...)
    }
    insert_into_db(..., conn = conn)
  }
# Or...
# insert_into_db_odds_tr <- 
#   purrr::partial(insert_into_db, conn = get_db_conn_odds_tr)

.import_odds_sport_tr <-
  function(table, ...) {
    # chkDots(...)
    conn <- get_db_conn_odds_tr()
    on.exit(drop_db_conn(conn))
    res <-
      read_from_db(
        conn = conn,
        table = table
      ) %>% 
      .convert_date_cols_at() %>% 
      .convert_time_cols_at() %>% 
      .convert_timestamp_cols_at()
    res
  }

import_odds_nfl_tr <-
  purrr::partial(.import_odds_sport_tr, table = 'odds_nfl_tr')
import_odds_nba_tr <-
  purrr::partial(.import_odds_sport_tr, table = 'odds_nba_tr')
