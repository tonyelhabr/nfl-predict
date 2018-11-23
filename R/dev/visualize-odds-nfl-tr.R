
odds_nfl_tr_exist <- import_odds_nfl_tr()

tm_colors <-
  teamcolors::teamcolors %>% 
  as_tibble() %>% 
  filter(league == "nfl") %>% 
  left_join(
    nfl_tm %>% 
      select(tm, tm_name_full), 
    by = c("name" = "tm_name_full")
  )
# tm_colors_pri <-
#   tm_colors %>% 
#   select(tm, primary) %>% 
#   deframe()
# tm_colors_sec <-
#   tm_colors %>% 
#   select(tm, secondary) %>% 
#   deframe()

.wk <- odds_nfl_tr_exist %>% tetidy::pull_distinctly(wk)
# .wk <- c(1L:10L)
data_filt <-
  odds_nfl_tr_exist %>%
  filter(wk %in% .wk)


data_proc <-
  data_filt %>% 
  gather(metric, value, matches("spread|total|moneyline")) %>% 
  separate(metric, into = c("metric", "side"), sep = "_") %>% 
  mutate(gm = sprintf("%s@%s", tm_home, tm_away)) %>% 
  filter(!str_detect(metric, "moneyline")) %>% 
  inner_join(
    tm_colors %>% select(tm_home = tm, color_home = primary)
  ) %>% 
  inner_join(
    tm_colors %>% select(tm_away = tm, color_away = primary)
  ) %>% 
  mutate_at(vars(wk), funs(sprintf("Week %02d", .))) %>% 
  mutate_at(
    vars(timestamp_scrape),
    funs(timestamp_scrape_pretty = strftime(., "%a %I %p"))
  ) %>%
  select(
    timestamp_scrape,
    timestamp_scrape_pretty,
    season,
    wk,
    # weekday,
    # date,
    gm,
    tm_home,
    tm_away,
    color_home,
    color_away,
    metric,
    value
  )
data_proc
.metric <- "spread"

# # Reference: http://rpubs.com/jbkunst/higcharter-grid-test.
# library("highcharter")
# purrr::map(
#   # c(4L:9L),
#   .wk,
#   function(x) {
#     data_proc %>%
#       filter(metric == .metric) %>% 
#       filter(wk == x) %>% 
#       hchart(
#         "line",
#         hcaes(x = timestamp_scrape_pretty, y = value, group = gm),
#         showInLegend = FALSE
#       ) %>% 
#       hc_title(text = paste0("Week ", x)) %>% 
#       hc_yAxis(title = list(text = ""))
#   }) %>% 
#   hw_grid(rowheight = 600, ncol = 2) %>%
#   htmltools::browsable()
# # Reference: https://plot.ly/r/subplots/.
# library("plotly")
# ggplot2::economics %>%
#   tidyr::gather(variable, value, -date) %>%
#   mutate(id = as.integer(factor(variable))) %>% 
#   count(id, variable, sort = TRUE)
# 
# ggplot2::economics %>%
#   tidyr::gather(variable, value, -date) %>%
#   mutate(id = as.integer(factor(variable))) %>% 
#   plot_ly(x = ~date, y = ~value, color = ~variable, colors = "Dark2",
#                                                       yaxis = ~paste0("y", id)) %>%
#   add_lines() %>%
#   # subplot(nrows = 5, shareX = TRUE)
#   subplot(nrows = 1, ncols = 1, shareX = TRUE, shareY = TRUE)
# viz_ly <-
#   data_proc %>% 
#   filter(metric == .metric) %>% 
#   plotly::plot_ly(
#     x = ~timestamp_scrape_pretty,
#     y = ~value,
#     color = ~tm_home,
#     yaxis = ~gm
#   ) %>%
#   plotly::add_lines() %>%
#   plotly::subplot(nrows = 10, shareX = TRUE)
# viz_ly

viz_odds <-
  data_proc %>% 
  filter(metric == .metric) %>% 
  ggplot(aes(x = timestamp_scrape, y = value)) +
  geom_hline(aes(yintercept = 0L), size = 1) +
  scale_x_datetime(date_labels = "%a %I %p") +
  geom_line(aes(color = color_home, group = gm), size = 1) +
  scale_color_identity() +
  # geom_point(aes(color = tm_away), size = 1.5) +
  # scale_color_manual(values = tm_colors_pri) +
  facet_wrap(~wk, scales = "free") +
  teplot::theme_te() +
  theme(
    panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # strip.text = element_blank()
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL
  )
viz_odds
viz_odds_ly <-
  viz_odds %>% 
  plotly::ggplotly()
viz_odds_ly
