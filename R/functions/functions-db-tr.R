
# get_db_conn_odds_tr <-
#   purrr::partial(get_db_conn, path = config::get("path_odds_tr"))

get_db_conn_odds_tr <-
  function(..., path) {
    if(missing(path)) {
      path <- config::get("path_odds_tr")
    }
    get_db_conn(path)
  }

# insert_into_db_odds_tr <- 
#   purrr::partial(insert_into_db, conn = get_db_conn_odds_tr)

insert_into_db_odds_tr <- 
  function(..., conn) {
    if(missing(conn)) {
      conn <- get_db_conn_odds_tr(...)
    }
    insert_into_db(..., conn = conn)
  }

.import_odds_sport_tr <-
  function(table, ...) {
    # chkDots(...)
    conn <- get_db_conn_odds_tr()
    on.exit(drop_db_conn(conn))
    read_from_db(
      conn = conn,
      table = table
    ) %>% 
      .convert_date_cols_at() %>% 
      .convert_time_cols_at() %>% 
      .convert_timestamp_cols_at()
    
  }

import_odds_nfl_tr <-
  purrr::partial(.import_odds_sport_tr, table = "odds_nfl_tr")
import_odds_nba_tr <-
  purrr::partial(.import_odds_sport_tr, table = "odds_nba_tr")

.fix_tm_col_sport_tr_at <-
  function(data,
           col,
           ...,
           col_suffix = c("home", "away"),
           col_prefix = "tm_",
           col_tr = "tm_name_tr",
           .data_source) {
    col_tr_sym <- sym(col_tr)
    
    tm_trim <-
      .data_source %>%
      filter(status == 1L) %>%
      select(tm, !!col_tr_sym)
    
    if(missing(col)) {
      col_suffix <- match.arg(col_suffix)
      col <- paste0(col_prefix, col_suffix)
    }
    col_sym <- sym(col)
    col_tr_new <- paste0(col, "_tr")
    col_tr_new_sym <- sym(col_tr_new)
    
    col_names_orig <- names(data)
    n_col_orig <- length(col_names_orig)
    idx_col <- match(col, names(data))
    seq_cols <- c(1:idx_col, (n_col_orig + 1), (idx_col + 1):n_col_orig)
    
    # browser()
    # NOTE: Not sure why, but can't use `col` in join clause.
    data %>%
      rename(!!col_tr_sym := !!col_sym) %>%
      inner_join(tm_trim, by = col_tr) %>%
      rename(!!col_sym := tm, !!col_tr_new_sym := !!col_tr_sym) %>%
      # select(-!!col_tr_sym)
      select(seq_cols)
  }

# .fix_tm_cols_nfl_tr_at <-
#   function(data, ...) {
#     data %>%
#       .fix_tm_col_nfl_tr_at(col_suffix = "home", ...) %>% 
#       .fix_tm_col_nfl_tr_at(col_suffix = "away", ...)
#   }

.fix_tm_cols_sport_tr_at <-
  function(data, ..., .data_source) {
    data %>%
      .fix_tm_col_sport_tr_at(col_suffix = "home", .data_source = .data_source, ...) %>% 
      .fix_tm_col_sport_tr_at(col_suffix = "away", .data_source = .data_source, ...)
  }

.fix_tm_cols_nfl_tr_at <-
  purrr::partial(.fix_tm_cols_sport_tr_at, .data_source = import_nfl_tm())

.fix_tm_cols_nba_tr_at <-
  purrr::partial(.fix_tm_cols_sport_tr_at, .data_source = import_nba_tm())
# .fix_tm_cols_nba_tr_at <- function(data, ..., .data_source = import_nba_tm()) {
#   .fix_tm_cols_sport_tr_at(
#     data = data,
#     .data_source = .data_source,
#     ...
#   )
# }

.finalize_odds_nba_tr <-
  function(data, ...) {
    data %>%
      .fix_tm_cols_nba_tr_at(...) %>%
      .add_timeperiod_cols_nba(...) %>%
      .reorder_cols_nba_at(...)
  }

.finalize_odds_nfl_tr <-
  function(data, ...) {
    data %>%
      .fix_tm_cols_nfl_tr_at(...) %>%
      .add_timeperiod_cols_nfl(...) %>%
      .reorder_cols_nfl_at(...)
  }

.finalize_odds_nba_tr <-
  function(data, ...) {
    data %>%
      .fix_tm_cols_nba_tr_at(...) %>%
      # .add_timeperiod_cols_nba(...) %>%
      .reorder_cols_nba_at(...)
  }

do_get_odds_nfl_tr <-
  function(...) {
    data_raw <- get_odds_nfl_tr(...)
    data_raw %>% 
      .finalize_odds_nfl_tr(...) %>% 
      .add_scrape_cols_at(...)
  }

do_get_odds_nba_tr <-
  function(...) {
    data_raw <- get_odds_nba_tr(...)
    data_raw %>% 
      .finalize_odds_nba_tr(...) %>% 
      .add_scrape_cols_at(...)
  }

