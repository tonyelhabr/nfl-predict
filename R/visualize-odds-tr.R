
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

# Reference: https://stackoverflow.com/questions/27594959/grouping-every-n-minutes-with-dplyr.
.round_to_hour_at <-
  function(data,
           col_round,
           col_value,
           interval = 12L,
           ...,
           f_sum = NULL,
           cols_grp = setdiff(names(data), col_value)) {
    stopifnot(
      is.data.frame(data)
    )
    stopifnot(
      is.character(col_round),
      is.character(col_value),
      is.character(cols_grp)
    )
    stopifnot(is.integer(interval), interval < 24L, interval > 0L)
    # f_sum <- match.arg(f_sum)
    col_round_sym <- sym(col_round)
    col_value_sym <- sym(col_value)
    cols_grp_syms <- syms(cols_grp)
    res <-
      data %>%
      mutate(
        !!col_round_sym :=
          lubridate::floor_date(!!col_round_sym, unit = "day") +
          ((
            floor(lubridate::hour(!!col_round_sym) / interval) * interval
          ) %>%
            as.integer() %>%
            lubridate::hours())
        
      )
    
    if(!is.null(f_sum)) {
      res <-
        res %>% 
        group_by(!!!cols_grp_syms) %>% 
        summarise_at(vars(!!col_value_sym), funs(f_sum(., na.rm = TRUE) %>% na_if(Inf))) %>% 
        ungroup() 
    }
    res
  }

round_timestamp_scrape_tohour12 <- function(...) {
  .round_to_hour_at(
    ...,
    col_round = "timestamp_scrape",
    col_value = "value",
    interval = 12L,
    f_sum = base::min
  )
}


odds_tr_exist <-
  bind_rows(
    odds_nba_tr_exist %>% mutate(lg = "nba"),
    odds_nfl_tr_exist %>% mutate(lg = "nfl")
  )

odds_tr_slim <-
  odds_tr_exist %>%
  inner_join(
    tm_colors %>% 
      select(tm_home = tm, color_home = primary, lg = league)
  ) %>% 
  gather(metric, value, matches("spread|total|moneyline")) %>%
  separate(metric, into = c("metric", "side"), sep = "_") %>% 
  filter(!str_detect(metric, "moneyline")) %>%
  mutate(gm = sprintf("%s @ %s", tm_home, tm_away)) %>%
  mutate_at(vars(wk), funs(sprintf("Week %02d", .))) %>%
  select(
    timestamp_scrape,
    lg,
    season,
    wk,
    gm,
    color_home,
    # color_away,
    metric,
    value
  )
odds_tr_slim

odds_tr_proc <-
  odds_tr_exist_slim %>% 
  distinct()  %>% 
  round_timestamp_scrape_tohour12() %>% 
  arrange(timestamp_scrape)

# # Debugging...
# odds_nfl_tr_proc %>% 
#   count(season, wk, gm, metric, sort = TRUE)


odds_nfl_tr_proc <-
  odds_tr_proc %>% 
  filter(lg == "nfl")
wk_nfl_last <- 
  odds_nfl_tr_proc %>% 
  distinct(wk) %>% 
  pull(wk) %>% 
  sort() %>% 
  rev() %>% 
  pluck(1)
wk_nfl_last

visualize_odds_nfl_tr <-
  function(data,
           .metric = c("spread", "total"),
           ...,
           add_hline = switch(.metric, spread = TRUE, total = FALSE),
           date_breaks = "2 days",
           legend.position = "none") {
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
      scale_x_datetime(date_labels = "%a", date_breaks = date_breaks) +
      geom_line(aes(color = color_home, group = gm), size = 1) +
      scale_color_identity() +
      facet_wrap(~ wk, scales = "free_x") +
      teplot::theme_te() +
      theme(
        # panel.grid.major = element_blank(),
        # legend.position = legend.position,
        axis.text.x = element_text(angle = 90)
      ) +
      labs(
        title = sprintf("NFL Weekly %ss, 2018", str_to_title(.metric)),
        caption = "By Tony ElHabr. Data source: teamrankings.com",
        x = NULL,
        y = NULL
      )
    viz
  }

viz_odds_nfl_tr_spread_last <-
  odds_nfl_tr_proc %>%
  filter(wk == wk_nfl_last) %>% 
  visualize_odds_nfl_tr(
    .metric = "spread",
    legend.position = "bottom",
    date_breaks = "12 hours"
  )
viz_odds_nfl_tr_spread_last

viz_odds_nfl_tr_spread <-
  odds_nfl_tr_proc %>%
  visualize_odds_nfl_tr("spread")
viz_odds_nfl_tr_spread

viz_odds_nfl_tr_total <-
  odds_nfl_tr_proc %>%
  visualize_odds_nfl_tr("total")
viz_odds_nfl_tr_total


.plotlify_proc_odds_nfl_tr <-
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
    ly <- .plotlify_proc_odds_nfl_tr(viz, ...)
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

export_png(viz_odds_nfl_tr_spread)
export_png(viz_odds_nfl_tr_total)
export_as_plotly_widget(viz_odds_nfl_tr_spread)
export_as_plotly_widget(viz_odds_nfl_tr_total)
