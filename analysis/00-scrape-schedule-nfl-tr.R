
# library(tidyverse)

# NOTE: Set `week` to the number before the number corresponding to week 1.
# (This should correspond to the "Week: All" and "Show: All" parameters
# on the page at https://www.teamrankings.com/nfl/schedules/season/.)
url <- 'https://www.teamrankings.com/nfl/schedules/season/?week=479' # ?week=445'
html <- url %>% xml2::read_html()
html
sched_list_raw <- html %>% rvest::html_table(header = FALSE)
sched_list_raw
nms <- paste0(1L:length(sched_list_raw))
nms

sched_raw <-
  sched_list_raw %>% 
  set_names(nms) %>%
  tibble(wk = nms, data = .)

sched <-
  sched_raw %>%
  mutate(data = map(data, ~ set_names(.x, c(
    'gm', 'time', 'location'
  )))) %>%
  unnest(cols = c(data)) %>%
  select(wk, time, gm, location) %>%
  as_tibble() %>% 
  mutate(
    dddmondd = case_when(
      !str_detect(gm, '\\@|(vs\\.)') ~ gm,
      TRUE ~ NA_character_
    )
  ) %>% 
  fill(dddmondd) %>% 
  filter(gm != dddmondd) 
sched

yr <- strftime(Sys.Date(), '%Y')

sched_tr <-
  sched %>%
  mutate(mondd =
           dddmondd %>% 
           str_replace_all('\\s+', ' ') %>% 
           str_replace_all('^[a-zA-z]{3}\\s', '')
         ) %>% 
  mutate(monddyyyy = mondd %>% paste0(' ', yr)) %>% 
  mutate(date = monddyyyy %>% lubridate::mdy()) %>% 
  mutate(weekday = date %>% lubridate::wday(label = TRUE)) %>% 
  separate(gm, into = c('tm_away', 'tm_home'), sep = '\\s+(\\@|vs\\.)\\s+') %>% 
  select(date, weekday, time, location, wk, tm_home, tm_away)
sched_tr

path_sched_tr <- file.path('data', sprintf('schedule-nfl-%s-tr.csv', yr))
sched_tr %>% teproj::export_path(path_sched_tr)

# clean ----
# nfl_tms_raw <-
#   file.path('data', 'db_nfl.xlsx') %>% 
#   readxl::read_excel(sheet = 'nfl_tm')
nfl_tms_raw <- import_nfl_tm()
nfl_tms_raw
nfl_tms_tr <-
  nfl_tms_raw %>%
  filter(status == 1) %>% 
  select(tm, tm_name_tr = tm_name_tr, tm_name_full)

.join_tms_tr_at <-
  function(data, col_suffix = 'away') {

    col <- paste0('tm_', col_suffix)
    col_sym <- sym(col)
    col_tr <- paste0('tm_', col_suffix, '_tr')
    col_tr_sym <- sym(col_tr)
    col_full <- paste0('tm_', col_suffix, '_full')
    col_full_sym <- sym(col_full)
    
    # browser()
    # NOTE: Not sure why, but can't use `col` in join clause.
    data %>%
      rename(tm_name_tr = !!col_sym) %>% 
      inner_join(nfl_tms_tr, by = 'tm_name_tr') %>% 
      rename(!!col_tr_sym := tm_name_tr, !!col_sym := tm, !!col_full_sym := tm_name_full)
  }

# NOTE: This format is to match the current format of the data returned by
# `import_nfl_game_result()` (with the possible exceoption of 
# the `game_result_id` column, which is a custom column (not sourced from ESPN,
# teamrankings.com, or any other source).
sched_final <-
  sched_tr %>%
  .join_tms_tr_at(col_suffix = 'away') %>% 
  .join_tms_tr_at(col_suffix = 'home') %>% 
  select(-matches('_tr$')) %>% 
  mutate(seasontype = 2, season = as.integer(!!yr), wk_season = wk) %>% 
  select(seasontype, season, wk, wk_season, tm_home, tm_away)
sched_final

path_sched <- file.path('data', sprintf('schedule-nfl-%s.csv', yr))
sched_final %>% teproj::export_path(path_sched)
  