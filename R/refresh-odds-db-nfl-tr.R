
conn <- get_db_conn_odds_tr()

odds_tr_exist <-
  import_odds_tr()
odds_tr_exist %>% 
  arrange(desc(timestamp_scrape))
.wk <- 2
odds_tr_lag1 <-
  odds_tr_exist %>% 
  filter(wk == .wk) %>% 
  mutate(gm = sprintf("%s@%s", tm_home, tm_away)) %>% 
  group_by(tm_home) %>% 
  filter(row_number(desc(timestamp_scrape)) == 1L) %>% 
  ungroup() %>% 
  select(tm_home, tm_away, spread_home, total) %>% 
  .arrange_gm_nfl()
odds_tr_lag1 %>% teproj::export_path(config$path_odds_lag1_nfl_temp)

.wk <- 3L
library("teplot")
tm_colors <-
  teamcolors::teamcolors %>% 
  as_tibble() %>% 
  filter(league == "nfl") %>% 
  left_join(nfl_tm %>% select(tm, tm_name_full), by = c("name" = "tm_name_full"))
tm_colors_pri <-
  tm_colors %>% 
  select(tm, primary) %>% 
  deframe()
tm_colors_sec <-
  tm_colors %>% 
  select(tm, secondary) %>% 
  deframe()
viz_odds <-
  odds_tr_exist %>% 
  filter(wk == .wk) %>% 
  gather(metric, value, matches("spread|total|moneyline")) %>% 
  separate(metric, into = c("metric", "side"), sep = "_") %>% 
  mutate(gm = sprintf("%s@%s", tm_home, tm_away)) %>% 
  # filter(str_detect(tm_away, "DAL")) %>% 
  group_by(gm, metric) %>% 
  mutate_at(vars(value), funs((. - min(., na.rm = T)) / (max(., na.rm = T) - min(., na.rm = T)))) %>% 
  ungroup() %>% 
  filter(!str_detect(metric, "moneyline")) %>% 
  ggplot(aes(x = timestamp_scrape, y = value, group = gm)) +
  geom_line(aes(color = tm_home, fill = tm_home), size = 1.5) +
  scale_color_manual(values = tm_colors_pri) +
  scale_fill_manual(values = tm_colors_sec) +
  facet_wrap(~metric, scales = "free", ncol = 2) +
  teplot::theme_te() +
  theme(
    # strip.text = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL
  )
viz_odds

# DBI::dbCreateTable(con = conn, "odds_tr", odds_tr)
DBI::dbListTables(conn = conn)
# DBI::dbRemoveTable(conn = conn, "nfl_game_odds")
# odds_tr %>% get_distinct_tms_at()

odds_tr_distinct <-
  odds_tr_exist %>%
  # mutate_at(vars(timestamp_scrape, timestamp_record), funs(as.POSIXct)) %>% 
  distinctify_data_at()
odds_tr_distinct

# odds_tr_distinct %>% 
odds_tr_exist %>% 
  .reorder_cols_nfl_at() %>% 
  # mutate(id_record = row_number()) %>% 
  insert_into_db(
    conn = conn,
    table = "odds_tr",
    overwrite = TRUE
  )
drop_db_conn(conn)
