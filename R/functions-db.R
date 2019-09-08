
get_db_conn <-
  function(path) {
    DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  }

create_table <-
  function(data,
           conn,
           table = deparse(substitute(data)),
           ...) {
    insert_into_db(
      data = data,
      conn = conn,
      table = table,
      overwrite = TRUE,
      append = FALSE,
      ...
    )
  }

.var_not_sqlite_compatible <-
  function(x) {
    !is.character(x) & !is.numeric(x)
  }

.create_backup <-
  function(path, suffix_backup = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")) {
    stopifnot(file.exists(path))
    file <- tools::file_path_sans_ext(path)
    ext <- tools::file_ext(path)
    path_backup <-
      sprintf("%s-%s.%s", file, suffix_backup, ext)
    if(file.exists(path_backup)) {
      msg <- sprintf("Backup file %s already exists! Are you sure you want to overwrite it?", path_backup)
      stop(msg, call. = FALSE)
      return(FALSE)
    }
    file.copy(from = path, to = path_backup)
    path_backup
  }

insert_into_db <-
  function(data,
           conn,
           table = deparse(substitute(data)),
           overwrite = config::get("overwrite"),
           append = !overwrite,
           backup = overwrite,
           ...,
           add_record_cols = TRUE,
           col_timestamp = "timestamp_record",
           col_id = "id_record",
           timestamp = format(Sys.time(), "%F %X"),
           verbose = TRUE) {
    # if(missing(get_db_conn)) {
    #   conn <- get_db_conn()
    # }
    # browser()
    if(is.null(data)) {
      if(verbose) {
        msg <- "Not inserting into db because `data` is `NULL`."
        message(msg)
      }
      return(data)
    }
    e <- DBI::dbExistsTable(con = conn, name = table)
    if(!e) {
      if(overwrite) {
        if(verbose) {
          msg <- sprintf("No table %s exists. Creating it.", table)
          message(msg)
        }
      } else {
        if(verbose) {
          msg <-
            sprintf(
              paste0(
                "No table %s exists. ",
                "Do you mean to create it? (Set `overwrite = TRUE` to create it.)"
              ), table
            )
          message(msg)
        }
        return(NULL)
      }
    }
    
    if(backup) {
      # NOTE: Moved this from outside the `overwrite` clause so it is done no matter what.
      path_db <- conn@dbname
      path_db_backup <- .create_backup(path = path_db)
      if(verbose) {
        msg <- sprintf("Backing up database as %s as a precaution.", path_db_backup)
        message(msg)
      }
    }
    
    if(!overwrite) {
      if(add_record_cols) {
        
        # NOTE: Make sure that these columns are NOT already in the data to be inserted.
        stopifnot(!(all(c(col_timestamp, col_id) %in% names(data))))
        col_timestamp <- sym(col_timestamp)
        col_id <- sym(col_id)
        
        data <-
          data %>%
          mutate(!!col_timestamp := timestamp)
        
        if(e) {
          data_read <-
            DBI::dbReadTable(
              conn = conn,
              name = table
            )
          
          val_id_max <-
            data_read %>% 
            summarise(max = max(!!col_id, na.rm = TRUE)) %>% 
            pull(max)
          
        } else {
          val_id_max <- 0
          
        }
        # browser()
        data <-
          data %>%
          mutate(!!col_id := val_id_max + row_number())
      }
    } else {
      
      if(add_record_cols) {
        if(e) {
          data_read <-
            DBI::dbReadTable(
              conn = conn,
              name = table
            )
          
          # NOTE: Make sure that these columns are in the database table.
          stopifnot(all(c(col_timestamp, col_id) %in% names(data_read)))
        }
      }
    }
    
    
    data <-
      data %>%
      mutate_if(.var_not_sqlite_compatible, as.character) %>% 
      as.data.frame()
    
    msg <- sprintf("Writing to table %s at %s.", table, Sys.time())
    message(msg)
    DBI::dbWriteTable(
      conn = conn,
      name = table,
      value = data,
      overwrite = overwrite,
      append = append,
      ...
    )
  }

drop_db_conn <-
  function(conn) {
    DBI::dbDisconnect(conn)
  }

read_from_db <-
  function(data,
           conn,
           table) {
    e <- DBI::dbExistsTable(con = conn, name = table)
    if(!e) {
      msg <- sprintf("No table %s exists.", table)
      warning(msg, call. = FALSE)
      return(NULL)
    }
    DBI::dbReadTable(
      conn = conn,
      name = table
    ) %>% 
      as_tibble()
  }

# TODO: `remove_from_db()`?
