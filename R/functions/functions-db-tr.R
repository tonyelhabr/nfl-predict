
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

import_odds_tr <-
  function() {
    # chkDots(...)
    conn <- get_db_conn_odds_tr()
    on.exit(drop_db_conn(conn))
    read_from_db(
      conn = conn,
      table = "odds_tr"
    ) %>% 
      .convert_date_cols_at() %>% 
      .convert_time_cols_at() %>% 
      .convert_timestamp_cols_at()
    
  }

.fix_tm_col_nfl_tr_at <-
  function(data,
           col,
           ...,
           col_suffix = c("home", "away"),
           col_prefix = "tm_",
           col_tr = "tm_name_tr") {
    
    col_tr_sym <- sym(col_tr)
    
    nfl_tm_trim <-
      import_nfl_tm() %>%
      filter(status == 1L) %>%
      select(tm, !!col_tr_sym)
    
    if(missing(col)) {
      col_suffix <- match.arg(col_suffix)
      col <- paste0(col_prefix, col_suffix)
    }
    col_sym <- sym(col)
    col_tr_new <- paste0(col, "_tr")
    col_tr_new_sym <- sym(col_tr_new)
    
    # browser()
    col_names_orig <- names(data)
    n_col_orig <- length(col_names_orig)
    idx_col <- match(col, names(data))
    seq_cols <- c(1:idx_col, (n_col_orig + 1), (idx_col + 1):n_col_orig)
    
    # browser()
    # NOTE: Not sure why, but can't use `col` in join clause.
    data %>%
      rename(!!col_tr_sym := !!col_sym) %>%
      inner_join(nfl_tm_trim, by = col_tr) %>%
      rename(!!col_sym := tm, !!col_tr_new_sym := !!col_tr_sym) %>%
      # select(-!!col_tr_sym)
      select(seq_cols)
  }

.fix_tm_cols_nfl_tr_at <-
  function(data, ...) {
    data %>%
      .fix_tm_col_nfl_tr_at(col_suffix = "home", ...) %>% 
      .fix_tm_col_nfl_tr_at(col_suffix = "away", ...)
  }

.add_timeperiod_cols_nfl_tr <-
  function(data, ..., season = config::get()$season_current) {
    .season <- season
    nfl_game_result_trim <-
      import_nfl_game_result() %>% 
      filter(season == .season) %>% 
      select(season, wk, tm_home, tm_away)

    col_names_out <- c("season", "wk", names(data))
    data %>%
      left_join(
        nfl_game_result_trim,
        by = c("tm_home", "tm_away")
      ) %>% 
      select(one_of(col_names_out))
  }


.finalize_odds_nfl_tr <-
  function(data, ...) {
    data %>%
      .fix_tm_cols_nfl_tr_at(...) %>%
      .add_timeperiod_cols_nfl_tr(...) %>%
      .reorder_cols_nfl_at(...)
  }

do_get_odds_nfl_tr <-
  function(...) {
    data_raw <- get_odds_nfl_tr(...)
    data_raw %>% 
      .finalize_odds_nfl_tr(...) %>% 
      .add_scrape_cols_at(...)
  }

