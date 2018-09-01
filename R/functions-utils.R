
.TIME <- Sys.time()
.get_ymdhm <-
  function(time = .TIME, resolution = 1L) {
    stopifnot(lubridate::is.POSIXct(time))
    stopifnot(is.integer(resolution))
    m1 <- as.numeric(strftime(round.POSIXt(time, "mins"), "%M"))
    m2 <- resolution * round((m1 - resolution - 1L) / resolution)
    # ymdh <- strftime(time, "%Y%m%d%H")
    ymd <- strftime(time, "%Y-%m-%d")
    h <- strftime(time, "%H")
    # ymdh_round <- sprintf("%s%02.0f00", ymdh, m2)
    ymdh_round <- sprintf("%s %s:%02.0f:00", ymd, h, m2)
    lubridate::ymd_hms(ymdh_round)
  }

.add_timestamp_scrape_col_at <-
  function(data, col = "timestamp_scrape", ...) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1, !(col %in% names(data)))
    col <- sym(col)
    data %>%
      mutate(!!col := .get_ymdhm(...))
  }

# .add_date_scrape_col_at <-
#   function(data, col = "date_scrape", ...) {
#     stopifnot(is.data.frame(data))
#     stopifnot(is.character(col), length(col) == 1, !(col %in% names(data)))
#     col <- sym(col)
#     data %>%
#       mutate(!!col := lubridat::ymd(Sys.Date()))
#   }


add_scrape_cols_at <-
  function(data, ...) {
    data %>%
      # .add_date_scrape_col_at() %>% 
      .add_timestamp_scrape_col_at()
  }

convert_date_cols_at <-
  function(data, cols = str_subset(names(data), "date")) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(cols), all(cols %in% names(data)))
    cols <- syms(cols)
    data %>%
      mutate_at(vars(!!!cols), funs(lubridate::ymd))
  }

convert_time_cols_at <-
  function(data, cols = str_subset(names(data), "time")) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(cols), all(cols %in% names(data)))
    cols <- syms(cols)
    data %>%
      mutate_at(vars(!!!cols), funs(lubridate::ymd_hms))
  }

convert_timestamp_cols_at <-
  function(data, cols = str_subset(names(data), "timestamp")) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(cols), all(cols %in% names(data)))
    convert_time_cols_at(
      data = data,
      cols = cols
    )
  }

add_timestamp_scrape_cols_at <-
  function(data, col = "timestamp_scrape") {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1, col %in% names(data))
    # cls <- data %>% pull(!!sym(col)) %>% class()
    # stopifnot(cls == "POSIXct")
    stopifnot(data %>% pull(!!sym(col)) %>% lubridate::is.POSIXct())
    data %>%
      mutate(
        year_scrape = lubridate::year(timestamp_scrape),
        month_scrape = lubridate::month(timestamp_scrape),
        day_scrape = lubridate::day(timestamp_scrape),
        hour_scrape = lubridate::hour(timestamp_scrape),
        minute_scrape = lubridate::minute(timestamp_scrape),
        second_scrape = lubridate::second(timestamp_scrape)
      )
  }


get_distinct12_at <-
  function(data, col, prefix = NULL, suffix = NULL) {
    
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1, !(col %in% names(data)))
    col1 <- col
    col2 <- col
    if(!is.null(prefix)) {
      stopifnot(length(prefix) == 2, is.character(prefix), prefix[1] != prefix[2])
      col1 <- paste0(prefix[1], col1)
      col2 <- paste0(prefix[2], col2)
    }
    if(!is.null(suffix)) {
      stopifnot(length(suffix) == 2, is.character(suffix), suffix[1] != suffix[2])
      col1 <- paste0(col, suffix[1])
      col2 <- paste0(col, suffix[2])
    }
    stopifnot(col1 %in% names(data), col2 %in% names(data))

    col <- sym(col)
    col1 <- sym(col1)
    col2 <- sym(col2)
    bind_rows(
      data %>%
        select(temp = !!col1) %>%
        distinct(temp),
      data %>%
        select(temp = !!col2) %>%
        distinct(temp)
    ) %>%
      filter(!is.na(temp)) %>%
      distinct(temp) %>%
      arrange(temp) %>%
      rename(!!col := temp)
  }

get_distinct_tms_at <-
  function(data, col = "tm", suffix = c("_away", "_home")) {
    get_distinct12_at(
      data = data,
      col = col,
      suffix = suffix
    )
  }

distinctify_data_at <-
  function(data, unit = "day", col = "timestamp_scrape", rgx_exclude = "record") {

    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1, col %in% names(data))
    lvls_time <-
      c("year", "month", "day", "hour", "minute", "second")
    # stopifnot(unit %in% lvls_time)
    unit <- match.arg(arg = unit, choices = lvls_time, several.ok = FALSE)
    fcts_time <-
      factor(lvls_time, levels = lvls_time)
    idx_slice <- which(unit == fcts_time)
    fcts_sliced <- as.character(fcts_time[c(1L:idx_slice)])
    rgx_fcts_sliced <- paste("^", fcts_sliced, sep = "", collapse = "|")
    rgx_fcts_sliced_scrape <- paste("^", fcts_sliced, "_scrape", sep = "", collapse = "|")


    data_aug <-
      data %>%
      add_timestamp_scrape_cols_at(col = col)
    
    cols_fcts_sliced <-
      data_aug %>% 
      names() %>% 
      str_subset(rgx_fcts_sliced)
    
    cols_fcts_sliced_scrape <-
      data_aug %>% 
      names() %>% 
      str_subset(rgx_fcts_sliced_scrape)
    
    cols_include <-
      data %>%
      names() %>% 
      setdiff(col)
    
    if(!is.null(rgx_exclude)) {
      stopifnot(is.character(rgx_exclude), length(rgx_exclude) == 1)
      cols_include <-
        cols_include %>%
        setdiff(str_subset(names(data_aug), rgx_exclude))
    }

    cols_distinct <- c(cols_include, cols_fcts_sliced, cols_fcts_sliced_scrape)
    cols_select <- names(data)
    data_aug %>% 
      distinct(!!!syms(cols_distinct), .keep_all = TRUE) %>% 
      select(!!!syms(cols_select)) %>% 
      inner_join(data)
  }

