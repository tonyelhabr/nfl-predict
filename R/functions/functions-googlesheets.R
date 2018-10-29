

.VERBOSE <- FALSE
.ASSIGN <- TRUE

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
  function(..., .name = config$file_db_nfl, .f = .import_db_nfl) {
    .import_file_ifnecessary(..., .name = .name, .f = .f)
  }

.import_db_nba_ifnecessary <-
  function(..., .name = config$file_db_nba, .f = .import_db_nba) {
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
           key = config$key_db_nfl,
           verbose = .VERBOSE,
           assign = .ASSIGN,
           .name = config$file_db_nfl) {
    .import_gs_file(..., key = key, verbose = verbose, assign = assign, .name = .name)
  }

.import_db_nba <-
  function(...,
           key = config$key_db_nba,
           verbose = .VERBOSE,
           assign = .ASSIGN,
           .name = config$file_db_nba) {
    .import_gs_file(..., key = key, verbose = verbose, assign = assign, .name = .name)
  }

.import_db_sports_local_sheet_ifnecessary <-
  function(..., path = config$path_db_sports_local, ws, verbose = .VERBOSE, assign = .ASSIGN, .name = ws) {
    if (exists(ws, envir = .GlobalEnv)) {
      return(invisible(get(ws, envir = .GlobalEnv)))
    }
    stopifnot(config$local)
    stopifnot(file.exists(path))
    sheets <- readxl::excel_sheets(path)
    stopifnot(ws %in% sheets)
    data <- readxl::read_excel(path, sheet = ws, ...)
    if (assign) {
      assign(.name, data, envir = .GlobalEnv)
      if (verbose) {
        msg <- sprintf("%s assigned to Global Environment.", .name)
        message(msg)
      }
    }
    invisible(data)
  }

.import_db_sports_local_sheet <- .import_db_sports_local_sheet_ifnecessary

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

.import_nba_gs_sheet_ifnecessary <-
  function(...,
           ws,
           verbose = .VERBOSE,
           assign = .ASSIGN) {
    file <- .import_db_nba_ifnecessary(...)
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

.import_nba_gs_sheet <-
  function(..., ws) {
    .import_nba_gs_sheet_ifnecessary(..., ws = ws)
  }

import_nfl_tm <-
  function(..., ws = config$ws_nfl_tm) {
    if(config$local) {
      res <- .import_db_sports_local_sheet(..., ws = ws)
    } else {
      res <- .import_nfl_gs_sheet(..., ws = ws)
    }
  }

import_nfl_game_result <-
  function(..., ws = config$ws_nfl_game_result) {
    if(config$local) {
      res <- .import_db_sports_local_sheet(..., ws = ws)
    } else {
      res <- .import_nfl_gs_sheet(..., ws = ws)
    }
  }


import_nba_tm <-
  function(..., ws = config$ws_nba_tm) {
    if(config$local) {
      res <- .import_db_sports_local_sheet(..., ws = ws)
    } else {
      res <- .import_nba_gs_sheet(..., ws = ws)
    }
  }

