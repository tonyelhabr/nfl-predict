
path <- file.path("data-raw", "srs-example.csv")
data_raw <-
  path %>% 
  teproj::import_path_cleanly()
data_raw

tm <-
  data_raw %>% 
  get_distinct_tms_at() %>% 
  pull(tm)
tm
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
tm_pd <-
  tm %>%
  tibble(tm = .) %>% 
  mutate(data = purrr::map(tm, ~.summarise_f(data_raw, tm = .x))) %>% 
  select(-tm) %>% 
  unnest() %>% 
  group_by(tm1) %>% 
  mutate(n_frac = 1 / n()) %>% 
  select(tm1, tm2, pts_diff, n_frac)
tm_pd

tm_pd_summ <-
  tm_pd %>%
  group_by(tm1) %>% 
  summarise(
    pts_diff_mean = mean(pts_diff),
    n = n()
  ) %>% 
  ungroup()
tm_pd_summ

tm_pd_wide <-
  expand.grid(
    tm1 = tm,
    tm2 = tm,
    stringsAsFactors = FALSE
  ) %>% 
  as_tibble() %>% 
  left_join(tm_pd) %>% 
  # left_join(tm_pd_summ) %>% 
  mutate_at(vars(pts_diff), funs(as.double(.))) %>% 
  mutate_at(
    vars(pts_diff), 
    funs(
      case_when(
        tm1 == tm2 ~ -1,
        is.na(pts_diff) ~ 0,
        TRUE ~ n_frac
        # TRUE ~ .
      )
    ) 
  ) %>% 
  select(tm1, tm2, pts_diff) %>% 
  spread(tm2, pts_diff)
tm_pd_wide
m1 <-
  tm_pd_wide %>%
  select(-tm1) %>% 
  set_names(NULL) %>% 
  as.matrix()
m1
det(m1)
eigen(m1)$values
m2 <-
  tm_pd_summ %>% 
  mutate_at(vars(pts_diff_mean), funs(-.)) %>% 
  select(pts_diff_mean) %>% 
  set_names(NULL) %>% 
  as.matrix()
# m2
t(m2)
# m1 %*% m2
m3 <- solve(m1, m2)
m3

base::svd(m1)

all(round((m1 %*% m3), 2) == round(m2, 2))
m2

.m1 <- matrix(c(3L, 1L, 1L, 2L), byrow = TRUE, nrow = 2L)
.m1
.m2 <- matrix(c(9L, 8L), byrow = TRUE, nrow = 2L)
.m2
.m1 %*% .m2
.m3 <- solve(.m1, .m2)
.m3

.m1 %*% .m3
