
data_raw <-
  file.path("data-raw", "srs-example.csv") %>% 
  teproj::import_path_cleanly()
data_raw

tms <-
  data_raw %>% 
  get_distinct_tms_at()
tms
.summarise_f <-
  function(data, tm) {
    .tm <- tm
    data %>%
      filter(tm_home == .tm | tm_away == .tm) %>%
      mutate(is_tm_home = if_else(tm_home == tm, TRUE, FALSE)) %>%
      mutate(
        tm1 = if_else(is_tm_home, tm_home, tm_away),
        tm2 = if_else(is_tm_home, tm_away, tm_home)
      ) %>% 
      mutate(
        pts_diff = pts_home - pts_away
      ) %>% 
      mutate_at(vars(pts_diff), funs(if_else(is_tm_home, ., -.)))
  }
tms_pd <-
  tms %>%
  mutate(data = purrr::map(tm, ~.summarise_f(data_raw, tm = .x))) %>% 
  select(-tm) %>% 
  unnest() %>% 
  select(tm1, tm2, pts_diff)
tms_pd

tms_pd_mean <-
  tms_pd %>%
  group_by(tm1) %>% 
  summarise_at(vars(pts_diff), funs(mean(.))) %>% 
  ungroup()

tms_pd_wide <-
  tms_pd %>% 
  expand(tm1, tm2) %>% 
  left_join(tms_pd) %>% 
  left_join(tms_pd_mean %>% rename(pts_diff_mean = pts_diff)) %>% 
  mutate_at(
    vars(pts_diff), 
    funs(
      case_when(
        tm1 == tm2 ~ -pts_diff_mean,
        is.na(pts_diff) ~ as.double(0),
        TRUE ~ as.double(.)
      )
    ) 
  ) %>% 
  select(-pts_diff_mean) %>% 
  spread(tm2, pts_diff)
tms_pd_wide 
