
odds_nfl_tr_exist <- import_odds_nfl_tr()
nfl_tm <- import_nfl_tm()

odds_nba_tr_exist <- import_odds_nba_tr()
nba_tm <- import_nba_tm()

tm <-
  bind_rows(
    nfl_tm %>% filter(status == 1L) %>% select(tm, tm_name_full),
    nba_tm %>% select(tm, tm_name_full)
  )

tm_colors <-
  teamcolors::teamcolors %>%
  as_tibble() %>%
  # filter(league == "nfl") %>%
  inner_join(
    tm %>%
      select(tm, tm_name_full),
    by = c("name" = "tm_name_full")
  )
tm_colors
stopifnot(nrow(tm_colors) == 62L)

odds_nfl_tr_viz <-
  odds_nfl_tr_exist %>%
  gather(metric, value, matches("spread|total|moneyline")) %>%
  separate(metric, into = c("metric", "side"), sep = "_") %>%
  mutate(gm = sprintf("%s @ %s", tm_home, tm_away)) %>%
  filter(!str_detect(metric, "moneyline")) %>%
  inner_join(tm_colors %>% select(tm_home = tm, color_home = primary)) %>%
  inner_join(tm_colors %>% select(tm_away = tm, color_away = primary)) %>%
  mutate_at(vars(wk), funs(sprintf("Week %02d", .))) %>%
  select(
    timestamp_scrape,
    season,
    wk,
    gm,
    color_home,
    color_away,
    metric,
    value
  )
odds_nfl_tr_viz

visualize_odds_nfl_tr <-
  function(data,
           .metric = c("spread", "total"),
           ...,
           add_hline = switch(.metric, spread = TRUE, total = FALSE)) {
    .metric <- match.arg(.metric)
    data_filt <-
      data %>%
      filter(metric == .metric)
    
    viz <-
      data_filt %>%
      ggplot(aes(x = timestamp_scrape, y = value))
    
    if (add_hline) {
      viz <-
        viz +
        geom_hline(aes(yintercept = 0L), size = 1, linetype = "dotted")
    }
    viz <-
      viz +
      scale_x_datetime(date_labels = "%a %I %p") +
      geom_line(aes(color = color_home, group = gm), size = 1) +
      scale_color_identity() +
      facet_wrap(~ wk, scales = "free") +
      teplot::theme_te() +
      theme(panel.grid.major = element_blank(),
            legend.position = "none") +
      labs(
        title = sprintf("NFL Weekly %ss, 2018", str_to_title(.metric)),
        caption = "By Tony ElHabr. Data source: teamrankings.com",
        x = NULL,
        y = NULL
      )
    viz
  }

.plotlify_viz_odds_nfl_tr <-
  function(viz, ...) {
    stopifnot(all(c("gg", "ggplot") %in% class(viz)))
    viz %>%
      plotly::ggplotly(tooltip = c("x", "y", "group"))
  }

# Reference: https://github.com/ramnathv/htmlwidgets/issues/299.
.export_plotly_widget <-
  function(viz,
           ...,
           file = deparse(substitute(viz)),
           dir = "figs",
           ext = "html",
           path_dummy = paste0(file, ".", ext),
           path = file.path(dir, path_dummy)) {
    stopifnot(all(c("plotly", "htmlwidget") %in% class(viz)))
    # viz <- plotly::as_widget(viz)
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    htmlwidgets::saveWidget(widget = viz, file = path_dummy)
    # browser()
    invisible(
      file.copy(
        from = path_dummy,
        to = path,
        overwrite = TRUE
      )
    )
    invisible(unlink(path_dummy))
    invisible(path)
  }

export_as_plotly_widget <-
  function(viz, ..., file = deparse(substitute(viz))) {
    if (file == ".") {
      message("`viz` should not be passed in with a pipe.")
    }
    ly <- .plotlify_viz_odds_nfl_tr(viz, ...)
    .export_plotly_widget(ly, file = file)
  }

export_png <- 
  purrr::partial(
    teproj::export_ext_png,
    dir = "figs",
    units = "in", 
    width = 10, 
    height = 10
  )

viz_odds_nfl_tr_spread <-
  odds_nfl_tr_viz %>%
  visualize_odds_nfl_tr("spread")
viz_odds_nfl_tr_spread

viz_odds_nfl_tr_total <-
  odds_nfl_tr_viz %>%
  visualize_odds_nfl_tr("total")
viz_odds_nfl_tr_total

export_png(viz_odds_nfl_tr_spread)
export_png(viz_odds_nfl_tr_total)
export_as_plotly_widget(viz_odds_nfl_tr_spread)
export_as_plotly_widget(viz_odds_nfl_tr_total)
