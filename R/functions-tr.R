
# old ----
# .get_url_nfl <-
#   function(week = 1L,
#            ...,
#            url_base = 'https://www.teamrankings.com/',
#            url_nfl_suffix = 'nfl-odds-week-') {
#     stopifnot(is.numeric(week), (week > 0 & week < 18))
#     sprintf('%s%s%s', url_base, url_nfl_suffix, week)
#   }
# 
# url <- .get_url_nfl(week = 1)

# works, but not a function ----
# req <-
#   httr::POST(
#     url = 'https://www.teamrankings.com/ajax/league/v3/odds_controller.php',
#     encode = 'form',
#     httr::user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36'),
#     httr::add_headers(
#       `Referer` = 'https://www.teamrankings.com/nfl/odds/'
#     ),
#     body = list(
#       league = 'nfl',
#       view_type = 'odds',
#       view = 'latest',
#       period_id = 458,
#       season_id = 16,
#       picks_base_url = '/nfl/picks'
#     )
#   )
# req

.request_odds_tr <-
  function(league = c('nfl', 'nba', 'mlb', 'ncf', 'ncb'),
           view_type = 'odds',
           view = 'latest',
           period_id = NULL,
           season_id = NULL,
           picks_base_url = sprintf('/%s/picks', league),
           ...,
           url = 'https://www.teamrankings.com/ajax/league/v3/odds_controller.php',
           encode = 'form',
           user_agent = httr::user_agent(
             'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36'),
           headers = httr::add_headers(`Referer` = sprintf('https://www.teamrankings.com/%s/odds/', league))) {
    league <- match.arg(league, several.ok = FALSE)
    body <-
      list(
        league = league,
        view_type = view_type,
        view = view,
        season_id = season_id,
        picks_base_url = picks_base_url
      )
    # NOTE: It doesn't seem like the other leagues have this parameter.
    if(league %in% c('nfl', 'ncf')) {
      body <-
        c(body,
          list(period_id = period_id)
        )
    }
    
    httr::POST(
      url = url,
      encode = encode,
      user_agent,
      headers = headers,
      body = body
    )
  }

.request_odds_nfl_tr <-
  function(league = 'nfl',
           period_id = config$period_id_odds_tr_nfl,
           season_id = config$season_id_odds_tr_nfl,
           ...) {
    .request_odds_tr(
      league = league,
      period_id = period_id,
      season_id = season_id,
      ...
    )
  }

.request_odds_nba_tr <-
  function(league = 'nba',
           season_id = 216,
           ...) {
    .request_odds_tr(
      league = league,
      season_id = season_id,
      ...
    )
  }

# req_h <-
#   req_t %>%
#   str_split('\r\n') %>% 
#   unlist() %>% 
#   paste(sep = '', collapse = '\n')
# req_h

.extract_val_at_every <-
  function(v, offset = 0L, by = 11L) {
    v[seq.int(1L + offset, length(v), by = 11L)]
  }
.extract_tm_away <- purrr::partial(.extract_val_at_every, offset = 0L)
.extract_tm_home <- purrr::partial(.extract_val_at_every, offset = 5L)
.extract_spread_home <- purrr::partial(.extract_val_at_every, offset = 7L)
.extract_total_home <- purrr::partial(.extract_val_at_every, offset = 3L)
.extract_moneyline_home <- purrr::partial(.extract_val_at_every, offset = 9L)
.extract_moneyline_away <- purrr::partial(.extract_val_at_every, offset = 4L)

.parse_odds_sport_tr_bygame <-
  function(data_raw, idx = 1L) {
    
    data_idx <-
      data_raw %>% 
      pluck(idx)
    dddmonyyyy <-
      data_idx %>% 
      rvest::html_nodes('h2') %>%
      rvest::html_text() %>% 
      str_remove_all('(st|nd|rd|th)\\,') %>%
      str_replace_all('\\s+', ' ') %>% 
      str_remove_all('\\,')
    ymds <-
      dddmonyyyy %>% 
      strptime('%A %B %d %Y') %>% 
      lubridate::ymd()
    wds <-
      ymds %>%
      lubridate::wday(label = TRUE)

    times0 <-
      data_idx %>% 
      # rvest::html_nodes('tr th') %>% 
      rvest::html_nodes("tr [class='text-left']") %>% 
      rvest::html_text() %>% 
      str_trim()
    # Debugging...
    # ymds <- rep(lubridate::ymd('2018-09-23'), 2)
    # times0 <- c('1:00 PM EST', '4:25 PM EST')
    times <-
      purrr::map2_chr(ymds, times0, ~paste0(.x, ' ', .y)) %>% 
      lubridate::ymd_hm() %>%  
      lubridate::force_tz(tzone = 'America/New_York')
    txt <-
      data_idx %>% 
      rvest::html_nodes('tr td') %>% 
      rvest::html_text()
    
    tms_away <- .extract_tm_away(v = txt)
    tms_home <- .extract_tm_home(v = txt)
    spreads_home <- .extract_spread_home(v = txt)
    spreads_home[spreads_home == '(Pick)'] <- 0
    totals <- .extract_total_home(v = txt)
    moneylines_home <- .extract_moneyline_home(v = txt)
    moneylines_away <- .extract_moneyline_away(v = txt)
    gms <-
      tibble(
        date = ymds,
        time = times,
        weekday = wds,
        tm_home = tms_home,
        tm_away = tms_away,
        spread_home = spreads_home,
        total = totals,
        moneyline_home = moneylines_home,
        moneyline_away = moneylines_away
      )
    # gms <-
    #   gms %>% 
    #   mutate_at(
    #     vars(matches('spread|total|moneyline')), 
    #     funs(if_else(. != '--', as.numeric(.), NA_real_))
    #   )
    gms
  }

.parse_odds_sport_tr <-
  function(req, ..., verbose = TRUE) {
    # stopifnot(class(req) == 'response')
    txt <-
      req %>%
      httr::content(as = 'text')
    if(is.null(txt)) {
      if(verbose) {
        msg <- 'Returning nothing. (No games today?)'
        message(msg)
      }
      return(NULL)
    }
    html <-
      txt %>% 
      xml2::read_html()
    
    mods <-
      html %>% 
      rvest::html_nodes('.module')

    # TODO: Use `purrr::possibly()` here?
    suppressWarnings(
      data <-
        tibble(idx = 1L:length(mods)) %>% 
        mutate(
          data_parsed = 
            purrr::map(idx,
                       ~.parse_odds_sport_tr_bygame(data_raw = mods, idx = .x)
            )
        )
    )
    
    suppressWarnings(
      data <-
        data  %>% 
        unnest(data_parsed) %>% 
        mutate_at(
          vars(matches('spread|total|moneyline')), 
          funs(if_else(. != '--', as.numeric(.), NA_real_))
        ) %>% 
        select(-idx)
    )
    
    # TODO: Make this work.
    # if(verbose) {
    #   data_filt <-
    #     data %>%
    #     filter(is.na(spread_home) | is.na(total) | is.na(moneyline_home) | is.na(moneyline_away))
    #   
    #   if(nrow(data_filt) > 0L) {
    #     msg_format <- 'Missing odds data for %s @ %s on %s.'
    #     data_filt %>% 
    #       mutate(
    #         msg = purrr::pwalk(list(tm_away, tm_home, date),~sprintf(msg_format, ..1, ..2, ..3))
    #       ) %>% 
    #       pull(msg) %>%
    #       purrr::walk(message)
    #   }
    # }
    data
  }

.get_odds_sport_tr <-
  function(..., f_request, f_parse = NULL, verbose = TRUE) {
    f_request_possibly <- purrr::possibly(f_request, otherwise = NULL)
    res <- f_request_possibly(...)
    if(is.null(res)) {
      if(verbose) {
        msg <- 'Something went wrong.'
        message(msg)
      }
      return(NULL)
    }
    if(!is.null(f_parse)) {
      res <- res %>% f_parse(...)
    }
    res
  }

.get_odds_nfl_tr <-
  function(...) {
    .get_odds_sport_tr(
      f_request = .request_odds_nfl_tr,
      f_parse = .parse_odds_sport_tr,
      ...
    )
  }

.get_odds_nba_tr <-
  function(...) {
    .get_odds_sport_tr(
      f_request = .request_odds_nba_tr,
      f_parse = .parse_odds_sport_tr,
      ...
    )
  }

.fix_tm_col_sport_tr_at <-
  function(data,
           col,
           ...,
           col_suffix = c('home', 'away'),
           col_prefix = 'tm_',
           col_tr = 'tm_name_tr',
           .data_source) {
    col_tr_sym <- sym(col_tr)
    
    tm_trim <-
      .data_source %>%
      filter(status == 1) %>%
      select(tm, !!col_tr_sym)
    
    if(missing(col)) {
      col_suffix <- match.arg(col_suffix)
      col <- paste0(col_prefix, col_suffix)
    }
    col_sym <- sym(col)
    col_tr_new <- paste0(col, '_tr')
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

.fix_tm_cols_sport_tr_at <-
  function(data, ..., .data_source) {
    data %>%
      .fix_tm_col_sport_tr_at(col_suffix = 'home', .data_source = .data_source, ...) %>% 
      .fix_tm_col_sport_tr_at(col_suffix = 'away', .data_source = .data_source, ...)
  }

.fix_tm_cols_nfl_tr_at <-
  purrr::partial(.fix_tm_cols_sport_tr_at, .data_source = import_nfl_tm())

.fix_tm_cols_nba_tr_at <-
  purrr::partial(.fix_tm_cols_sport_tr_at, .data_source = import_nba_tm())

.postprocess_odds_nba_tr <-
  function(data, ...) {
    data %>%
      .fix_tm_cols_nba_tr_at(...) %>%
      .add_timeperiod_cols_nba(...) %>%
      .reorder_cols_nba_at(...)
  }

.postprocess_odds_nfl_tr <-
  function(data, ...) {
    data %>%
      .fix_tm_cols_nfl_tr_at(...) %>%
      .add_timeperiod_cols_nfl(...) %>%
      .reorder_cols_nfl_at(...) %>% 
      .add_scrape_cols_at(...)
  }

.postprocess_odds_nba_tr <-
  function(data, ...) {
    data %>%
      .fix_tm_cols_nba_tr_at(...) %>%
      # .add_timeperiod_cols_nba(...) %>%
      .reorder_cols_nba_at(...) %>% 
      .add_scrape_cols_at(...)
  }

.do_get_odds_sport_tr <-
  function(..., f_get, f_postprocess = NULL, verbose = TRUE) {
    # f_get_possibly <- purrr::possibly(f_get, otherwise = NULL)
    f_get_possibly <- f_get
    res <- f_get_possibly(...)
    if(is.null(res)) {
      if(verbose) {
        msg <- 'Something went wrong.'
        message(msg)
      }
      return(NULL)
    }
    if(!is.null(f_postprocess)) {
      res <- res %>% f_postprocess(...)
    }
    res
  }

do_get_odds_nfl_tr <-
  function(...,
           f_get = .get_odds_nfl_tr,
           f_postprocess = .postprocess_odds_nfl_tr) {
    .do_get_odds_sport_tr(
      f_get = f_get,
      f_postprocess = f_postprocess,
      ...
    )
  }

do_get_odds_nba_tr <-
  function(...,
           f_get = .get_odds_nba_tr,
           f_postprocess = .postprocess_odds_nba_tr) {
    .do_get_odds_sport_tr(
      f_get = f_get,
      f_postprocess = f_postprocess,
      ...
    )
  }


