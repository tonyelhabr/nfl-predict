
.import_db_nfl_ifnecessary <-
  function(...) {
    if(!exists("db_nfl", envir = .GlobalEnv)) {
      db_nfl <- .import_db_nfl(...)
    }
    db_nfl
  }

.import_nfl_sheet_ifnecessary <-
  function(..., ws) {
    if(exists(ws, envir = .GlobalEnv)) {
      return(get(ws, envir = .GlobalEnv))
    }
    db_nfl <- .import_db_nfl_ifnecessary(...)
    data <- googlesheets::gs_read(db_nfl, ws = ws)
    assign(ws, data, envir = .GlobalEnv)
    data
  }

.import_db_nfl <-
  function(..., key = config::get()$key_db_nfl) {
    # NOTE: Could do something like this.
    # if(!exists("config", .GlobalEnv)) {
    #   config <- config::get()
    # }
    db_nfl <- googlesheets::gs_key(key)
    assign("db_nfl", db_nfl, envir = .GlobalEnv)
  }

.import_nfl_sheet <-
  function(...,  ws) {
    .import_nfl_sheet_ifnecessary(..., ws = ws)
  }

import_nfl_tm <-
  function(..., ws = config::get()$ws_nfl_tm) {
    .import_nfl_sheet(..., ws = ws)
  }

import_nfl_game_result <-
  function(..., ws = config::get()$ws_nfl_game_result) {
    .import_nfl_sheet(..., ws = ws)
  }
