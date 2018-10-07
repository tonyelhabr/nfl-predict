

.VERBOSE <- FALSE
.ASSIGN <- TRUE
.FILE_DB_NFL <- config::get()$file_db_nfl

.import_file_ifnecessary <-
  function(..., .name, .f) {
    if (!exists(.name, envir = .GlobalEnv)) {
      res <- .f(...)
    } else {
      res <- get(.name, envir = .GlobalEnv)
    }
    invisible(res)
  }

.import_db_nfl_ifnecessary <-
  function(..., .name = .FILE_DB_NFL, .f = .import_db_nfl) {
    .import_file_ifnecessary(..., .name = .name, .f = .f)
  }

.import_gs_file <-
  function(...,
           key,
           verbose,
           assign,
           .name) {
    file <- googlesheets::gs_key(key, verbose = verbose)
    if (assign) {
      assign(.name, file, envir = .GlobalEnv)
      if (verbose) {
        msg <- sprintf("%s assigned to Global Environment.", .name)
        message(msg)
      }
    }
    invisible(file)
  }


.import_db_nfl <-
  function(...,
           key = config::get()$key_db_nfl,
           verbose = .VERBOSE,
           assign = .ASSIGN,
           .name = .FILE_DB_NFL) {
    .import_gs_file(..., key = key, verbose = verbose, assign = assign, .name = .name)
  }

.import_gs_sheet_ifnecessary <-
  function(..., file, ws, verbose, assign, .name = ws) {
    # browser()
    if (exists(ws, envir = .GlobalEnv)) {
      return(invisible(get(ws, envir = .GlobalEnv)))
    }
    
    data <-
      googlesheets::gs_read(file,
                            ws = ws,
                            col_types = readr::cols(),
                            verbose = verbose
      )
    if (assign) {
      assign(.name, data, envir = .GlobalEnv)
      if (verbose) {
        msg <- sprintf("%s assigned to Global Environment.", .name)
        message(msg)
      }
    }
    invisible(data)
  }

.import_nfl_gs_sheet_ifnecessary <-
  function(...,
           ws,
           verbose = .VERBOSE,
           assign = .ASSIGN) {
    file <- .import_db_nfl_ifnecessary(...)
    .import_gs_sheet_ifnecessary(
      ...,
      file = file,
      ws = ws,
      verbose = verbose,
      assign = assign
    )
  }

# .import_gs_sheet <-
#   function(...) {
#     .import_gs_sheet(...)
#   }

.import_nfl_gs_sheet <-
  function(..., ws) {
    .import_nfl_gs_sheet_ifnecessary(..., ws = ws)
  }

import_nfl_tm <-
  function(..., ws = config::get()$ws_nfl_tm) {
    .import_nfl_gs_sheet(..., ws = ws)
  }

import_nfl_game_result <-
  function(..., ws = config::get()$ws_nfl_game_result) {
    .import_nfl_gs_sheet(..., ws = ws)
  }
