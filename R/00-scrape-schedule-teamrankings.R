
library("tidyverse")
url <-
  "https://www.teamrankings.com/nfl/schedules/season/?week=445"
html <-
  url %>%
  xml2::read_html()

sched_list_raw <-
  html %>%
  rvest::html_table(header = FALSE)
nms <-
  paste0(1L:length(sched_list_raw))

sched_raw <-
  sched_list_raw %>% 
  set_names(nms) %>%
  tibble(wk = nms, data = .)

sched <-
  sched_raw %>%
  mutate(data = map(data, ~ set_names(.x, c(
    "gm", "time", "location"
  )))) %>%
  unnest() %>%
  select(wk, time, gm, location) %>%
  as_tibble() %>% 
  mutate(
    dddmondd = case_when(
      !str_detect(gm, "\\@|(vs\\.)") ~ gm,
      TRUE ~ NA_character_
    )
  ) %>% 
  fill(dddmondd) %>% 
  filter(gm != dddmondd) 
sched

yr <- strftime(Sys.Date(), "%Y")

sched_tr <-
  sched %>%
  mutate(mondd =
           dddmondd %>% 
           str_replace_all("\\s+", " ") %>% 
           str_replace_all("^[a-zA-z]{3}\\s", "")
         ) %>% 
  mutate(monddyyyy = mondd %>% paste0(" ", yr)) %>% 
  mutate(date = monddyyyy %>% lubridate::mdy()) %>% 
  mutate(weekday = date %>% lubridate::wday(label = TRUE)) %>% 
  separate(gm, into = c("tm_away", "tm_home"), sep = "\\s+(\\@|vs\\.)\\s+") %>% 
  select(date, weekday, time, location, wk, tm_home, tm_away)
sched_tr

path_sched_tr <- file.path("data", "schedule-nfl-2018-teamrankings.csv")
sched_tr %>%
  teproj::export_path(path_sched_tr)

# clean ----
nfl_tms_raw <-
  file.path("data", "db_nfl.xlsx") %>% 
  readxl::read_excel(sheet = "nfl_tm")
nfl_tms_raw
nfl_tms_tr <-
  nfl_tms_raw %>%
  select(tm, tm_name_tr = tm_name_teamrankings, tm_name_full)

.join_tms_tr_at <-
  function(data, col_suffix = "away") {

    col <- paste0("tm_", col_suffix)
    col_sym <- sym(col)
    col_tr <- paste0("tm_", col_suffix, "_tr")
    col_tr_sym <- sym(col_tr)
    col_full <- paste0("tm_", col_suffix, "_full")
    col_full_sym <- sym(col_full)
    
    # browser()
    # NOTE: Not sure why, but can't use `col` in join clause.
    data %>%
      rename(tm_name_tr = !!col_sym) %>% 
      inner_join(nfl_tms_tr, by = "tm_name_tr") %>% 
      rename(!!col_tr_sym := tm_name_tr, !!col_sym := tm, !!col_full_sym := tm_name_full)
  }

sched_xlsx <-
  sched_tr %>%
  .join_tms_tr_at(col_suffix = "away") %>% 
  .join_tms_tr_at(col_suffix = "home") %>% 
  select(-matches("_tr$"))

path_sched_xlsx <- file.path("data", "schedule-nfl-2018.csv")
sched_xlsx %>%
  teproj::export_path(path_sched_xlsx)
  