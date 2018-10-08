
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
        tm1 == tm2 ~ 1,
        is.na(pts_diff) ~ 0,
        TRUE ~ -n_frac
        # TRUE ~ .
      )
    ) 
  ) %>% 
  select(tm1, tm2, pts_diff) %>% 
  spread(tm2, pts_diff)
tm_pd_wide

a <-
  tm_pd_wide %>%
  select(-tm1) %>% 
  set_names(NULL) %>% 
  as.matrix()
a

b <-
  tm_pd_summ %>% 
  # mutate_at(vars(pts_diff_mean), funs(-.)) %>% 
  select(pts_diff_mean) %>% 
  set_names(NULL) %>% 
  as.matrix()
b

# method 1 ----
# x <- solve(a, b)
# x <- solve(pracma::rref(a), b)
x_1 <- solve(a, b, tol = 1e-19)
x_1

# method 2 ----
# Reference: https://stackoverflow.com/questions/19763698/solving-non-square-linear-system-with-r.
# det(a)
# eigen(a)$values
m1_svd <- svd(a)
m1_d0 <- m1_svd$d
# m1_d <- m1_d0
# m1_d[m1_d == min(m1_d)] <- 0
# m1_d
idx_repl <- which.min(m1_d0)

m1_diag0 <- diag(1 / m1_d0)
m1_diag <- m1_diag0
m1_diag[idx_repl, idx_repl] <- 0
m1_diag

# m1_svd$u
# m1_svd$v
# round(m1_svd$u %*% t(m1_svd$u), 2)
# round(m1_svd$v %*% t(m1_svd$v), 2)
# all(diag(round(m1_svd$u %*% t(m1_svd$u), 2) == 1))
m1_new <-
  m1_svd$v %*% m1_diag %*% t(m1_svd$u)
x_2 <- m1_new %*% b
x_2

round(a %*% x_1, 2)
round(a %*% x_2, 2)

round(b, 2)

round(x_1, 2)
round(x_2, 2)

# method 3 ----
# Reference: https://stackoverflow.com/questions/19763698/solving-non-square-linear-system-with-r.
MASS::ginv(a) %*% b


# Perl solution ----
# Reference: https://codeandfootball.wordpress.com/2011/04/12/issues-with-the-simple-ranking-system/.
# A = 2.93
# B = -7.19
# C = -6.19
# D = -0.82
# E = -3.07
# F = 7.18


# # checking `solve()` ----
# .a <- matrix(c(3L, 1L, 1L, 2L), byrow = TRUE, nrow = 2L)
# .a
# .b <- matrix(c(9L, 8L), byrow = TRUE, nrow = 2L)
# .b
# .a %*% .b
# .x <- solve(.a, .b)
# .x
# 
# .a %*% .x
