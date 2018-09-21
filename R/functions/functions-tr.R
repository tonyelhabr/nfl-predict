
# old ----
# .get_url_nfl <-
#   function(week = 1L,
#            ...,
#            url_base = "https://www.teamrankings.com/",
#            url_nfl_suffix = "nfl-odds-week-") {
#     stopifnot(is.numeric(week), (week > 0 & week < 18))
#     sprintf("%s%s%s", url_base, url_nfl_suffix, week)
#   }
# 
# url <- .get_url_nfl(week = 1)

# works, but not a function ----
# req <-
#   httr::POST(
#     url = "https://www.teamrankings.com/ajax/league/v3/odds_controller.php",
#     encode = "form",
#     httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36"),
#     httr::add_headers(
#       `Referer` = "https://www.teamrankings.com/nfl/odds/"
#     ),
#     body = list(
#       league = "nfl",
#       view_type = "odds",
#       view = "latest",
#       period_id = 458,
#       season_id = 16,
#       picks_base_url = "/nfl/picks"
#     )
#   )
# req

.request_odds_tr <-
  function(league = c("nfl", "nba", "mlb", "ncf", "ncb"),
           view_type = "odds",
           view = "latest",
           period_id = NULL,
           season_id = NULL,
           picks_base_url = sprintf("/%s/picks", league),
           ...,
           url = "https://www.teamrankings.com/ajax/league/v3/odds_controller.php",
           encode = "form",
           user_agent = httr::user_agent(
             "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36"),
           headers = httr::add_headers(`Referer` = sprintf("https://www.teamrankings.com/%s/odds/", league))) {
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
    if(league %in% c("nfl", "ncf")) {
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
  function(league = "nfl",
           period_id = 458,
           season_id = 16,
           ...) {
    .request_odds_tr(
      league = league,
      period_id = period_id,
      season_id = season_id,
      ...
    )
  }

# req_h <-
#   req_t %>%
#   str_split("\r\n") %>% 
#   unlist() %>% 
#   paste(sep = "", collapse = "\n")
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

.parse_odds_nfl_tr_bygame <-
  function(data_raw, idx = 1L) {
    
    # browser()
    data_idx <-
      data_raw %>% 
      pluck(idx)
    dddmonyyyy <-
      data_idx %>% 
      rvest::html_nodes("h2") %>%
      rvest::html_text() %>% 
      str_remove_all("(th|nd|rd)\\,") %>%
      str_replace_all("\\s+", " ") %>% 
      str_remove_all("\\,")
    ymds <-
      dddmonyyyy %>% 
      strptime("%A %B %d %Y") %>% 
      lubridate::ymd()
    wds <-
      ymds %>%
      lubridate::wday(label = TRUE)
    times0 <-
      data_idx %>% 
      # rvest::html_nodes("tr th") %>% 
      rvest::html_nodes("tr [class='text-left']") %>% 
      rvest::html_text() %>% 
      str_trim()
    # Debugging...
    # ymds <- rep(lubridate::ymd("2018-09-23"), 2)
    # times0 <- c("1:00 PM EST", "4:25 PM EST")
    times <-
      purrr::map2_chr(ymds, times0, ~paste0(.x, " ", .y)) %>% 
      lubridate::ymd_hm() %>%  
      lubridate::force_tz(tzone = "America/New_York")
    txt <-
      data_idx %>% 
      rvest::html_nodes("tr td") %>% 
      rvest::html_text()
    
    tms_away <- .extract_tm_away(v = txt)
    tms_home <- .extract_tm_home(v = txt)
    spreads_home <- .extract_spread_home(v = txt)
    spreads_home[spreads_home == "(Pick)"] <- 0
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
      ) %>% 
      mutate_at(vars(matches("spread|total|moneyline")), funs(as.numeric))
    gms
  }

.parse_odds_nfl_tr <-
  function(req) {
    # stopifnot(class(req) == "response")
    txt <-
      req %>%
      httr::content(as = "text")
    
    html <-
      txt %>% 
      xml2::read_html()
    
    mods <-
      html %>% 
      rvest::html_nodes(".module")
    
    data <-
      tibble(idx = 1L:length(mods)) %>% 
      mutate(data_parsed = 
               map(idx,
                   ~.parse_odds_nfl_tr_bygame(data_raw = mods, idx = .x))) %>% 
      unnest(data_parsed) %>% 
      select(-idx)
    data
  }

get_odds_nfl_tr <-
  function(...) {
    req <- .request_odds_nfl_tr(...)
    .parse_odds_nfl_tr(req)
  }

