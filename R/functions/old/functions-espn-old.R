
# unused ----
# .filter_odds_nfl_espn <-
#   function(data, ...) {
#     data %>%
#       .separate_cols_max_espn_at(col = "name") %>%
#       filter((name1 == "events" &
#         name2 == "shortName") |
#           (name1 == "events" &
#              name2 == "competitions" &
#              name3 == "date"
#           ) |
#         (
#           name1 == "events" &
#             name2 == "competitions" &
#             name3 == "odds" &
#             name4 %in% c("details", "overUnder")
#         ))
#   }
# 
# .clean_odds_nfl_espn <-
#   function(data, ...) {
#     data %>%
#       select(isodds = name3, odds_type = name4, value) %>%
#       mutate_at(vars(isodds), funs(if_else(is.na(.), FALSE, TRUE))) %>%
#       mutate(gm = if_else(!isodds, value, NA_character_)) %>%
#       fill(gm, .direction = "down") %>%
#       filter(gm != value) %>%
#       select(gm, odds_type, value) %>%
#       spread(odds_type, value) %>%
#       separate(gm, into = c("tm_away", "tm_home"), sep = "(\\s+\\@\\s+)|(\\s+vs.*\\s+)") %>%
#       separate(details, into = c("tm_favored", "spread"), sep = "\\s+") %>%
#       mutate_at(vars(spread, overUnder), funs(as.numeric)) %>%
#       mutate(spread_home = if_else(tm_home == tm_favored, spread, -spread)) %>%
#       select(-tm_favored, -spread) %>%
#       rename(total = overUnder) %>%
#       mutate_at(vars(total), funs(if_else(. <= 0, NA_real_, .)))
#   }
# 
# .get_path_export_scores_nfl_espn <-
#   function(season, seasontype, wk, ...) {
#     file.path("data-raw", paste0("scores-", season, "-espn-", format(Sys.time(), "%F_%T"), ".csv"))
#     # file.path("data-raw", paste0("scores-", season, "-espn.csv"))
#   }
# 
# do_get_odds_nfl_espn <-
#   function(wk, season = config::get()$season_current, seasontype = 2L, ..., arrange = TRUE) {
#     data_raw <-
#       .preprocess_do_get_xxx_nfl_espn(
#         wk = wk,
#         season = season,
#         seasontype = seasontype,
#         ...
#       )
# 
#     data_raw %>%
#       mutate(data = purrr::map(data, ~.filter_scores_nfl_espn(data = .x, ...))) %>%
#       mutate(data = purrr::map(data, ~.clean_scores_nfl_espn(data = .x, ...))) %>%
#       .postprocess_do_get_xxx_nfl_espn(..., arrange = arrange)
#   }
# 
# .get_calendar_nfl_espn <-
#   function(...) {
#     data_raw <- espn2::get_scores_nfl(year = 2006, limit = 500, as = "minimal")
#     
#     calendar_raw <-
#       data_raw %>%
#       .separate_cols_max_espn_at(col = "name")
#     filter(name1 == "leagues" & name2 == "calendar") %>%
#       filter(
#         name3 == "label" |
#           (name3 == "entries" &
#              name4 == "label") |
#           (name3 == "entries"  & name4 %in% c("startDate", "endDate"))
#       )
#     
#     seasontypeid_nfl_espn <-
#       .get_seasontypeid_nfl_espn()
#     
#     calendar_raw %>%
#       select(matches("name[1-4]|value")) %>% 
#       mutate(seasontype = if_else(is.na(name4), value, NA_character_)) %>% 
#       mutate(seasontypelabel = if_else(name4 == "label", value, NA_character_)) %>% 
#       mutate(datetime =
#                if_else(str_detect(name4, "Date"),
#                        str_replace_all(value, "(T0)", " ") %>% str_replace("Z$", ""), NA_character_)) %>% 
#       mutate(when = if_else(str_detect(name4, "Date"), str_replace(name4, "Date", ""), NA_character_)) %>% 
#       select(matches("seasontype|date|when")) %>% 
#       fill(seasontype, .direction = "down") %>% 
#       fill(seasontypelabel, .direction = "down") %>% 
#       filter(!is.na(seasontypelabel) & !is.na(datetime)) %>% 
#       # mutate(date = if_else(!is.na(datetime), datetime %>% str_replace_all("(^.*)\\s", "\\1") %>% lubridate::ymd()) %>% 
#       mutate(time = datetime %>% lubridate::ymd_hm()) %>% 
#       select(-datetime) %>% 
#       spread(when, time) %>% 
#       left_join(seasontypeid_nfl_espn, by = c("seasontype")) %>% 
#       mutate(season = if_else(
#         lubridate::month(start, label = FALSE) <= 2,
#         lubridate::year(start) - 1,
#         lubridate::year(start)
#       )) %>%
#       mutate_at(vars(season), funs(as.integer)) %>% 
#       mutate(wkseasontype = str_extract(seasontypelabel, "[0-9]+") %>% as.integer()) %>% 
#       mutate(wk = row_number(seasontypeid * season + wkseasontype)) %>% 
#       mutate_at(vars(wk), funs(if_else(!str_detect(seasontype, "Season"), NA_integer_, .))) %>% 
#       group_by(season) %>% 
#       mutate_at(vars(wk), funs(as.integer(. - min(., na.rm = TRUE) + 1L))) %>% 
#       ungroup() %>% 
#       select(season, wk, wkseasontype, seasontype, seasontypelabel, start, end) %>% 
#       arrange(start)
#   }
