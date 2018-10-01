
odds_tr <- import_odds_tr()
odds_tr

.season <- get_season_current()
.wk <- 4L

odds_tr_latest <-
  odds_tr %>%
  filter(season == .season, wk == .wk) %>% 
  group_by(season, wk, tm_home, tm_away) %>% 
  mutate(rn = row_number(desc(timestamp_scrape))) %>% 
  filter(rn == 1L) %>% 
  ungroup() %>% 
  select(-rn)
odds_tr_latest

sheets_odds_hist_tr <-
  config$path_trends_hist_tr %>%
  readxl::excel_sheets()
sheets_odds_hist_tr
sheet_combined <- "combined"
stopifnot(sheet_combined %in% sheets_odds_hist_tr)

combined_hist_tr <-
  config$path_trends_hist_tr %>%
  readxl::read_excel(sheet = sheet_combined) %>%
  # NOTE: Purposely ignore ties.
  mutate(n_gm = n_w_fav + n_l_fav) %>% 
  mutate(n_gm_frac = n_gm / sum(n_gm)) %>%
  # mutate(n_gm_prnk = percent_rank(n_gm)) %>% 
  mutate(n_gm_frac_adj = (n_gm_frac - min(n_gm_frac)) / (max(n_gm_frac) - min(n_gm_frac))) %>% 
  mutate(
    w_frac_fav = n_w_fav / n_gm, 
    w_frac_over = n_w_over / n_gm
  ) %>% 
  select(-matches("n.*(_fav$|_over$)")) %>% 
  arrange(desc(n_gm))
combined_hist_tr

# # Testing...
# combined_hist_tr %>%
#   arrange(desc(n_gm))
# n_gm <- 809
# n_w_fav <- 369
# n_w_over <- 386
# binom.test(n_w_fav, n_gm, n_w_fav / n_gm) %>% broom::tidy()
# binom.test(n_w_over, n_gm, n_w_over / n_gm) %>% broom::tidy()

odds_tr_aug <-
  odds_tr_latest %>%
  .select_cols_nfl_at() %>% 
  mutate(spread_abs = abs(spread_home)) %>%
  fuzzyjoin::fuzzy_left_join(
    combined_hist_tr,
    by = c(
      "spread_abs" = "spread_low",
      "spread_abs" = "spread_high",
      "total" = "total_low",
      "total" = "total_high"
    ),
    match_fun = list(`>=`, `<=`, `>=`, `<=`)
  )
odds_tr_aug

odds_tr_pick <-
  odds_tr_aug %>%
  mutate(
    tm_pick =
      case_when(
        spread_home == -spread_abs & w_frac_fav > 0.5 ~ tm_home,
        spread_home == spread_abs & w_frac_fav < 0.5 ~ tm_home,
        !is.na(spread_home) ~ tm_away,
        TRUE ~ NA_character_
      )
  ) %>%
  mutate(
    total_pick =
      case_when(
        w_frac_over > 0.5 ~ "O",
        w_frac_over < 0.5 ~ "U",
        w_frac_over == 0.5 ~ "E",
        TRUE ~ NA_character_
      )
  )

odds_tr_pick %>% glimpse()

odds_tr_pick %>% 
  select(matches("pick$"), matches("w_frac"), one_of(.COLS_NFL_ORDER)) %>% 
  mutate_at(vars(matches("w_frac")), funs(rnk = row_number(desc(.)))) %>% 
  select(matches("pick$"), matches("w_frac"), matches("rnk$"))

# cols_orig <- odds_tr_latest %>% names()
# odds_tr_pick %>% select(one_of(cols_orig), matches("_pick$"))
odds_tr_pick %>%
  select(season, wk, tm_home, tm_away, spread_home, total, moneyline_home, moneyline_away, tm_pick, total_pick)



