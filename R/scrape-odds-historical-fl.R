
# TODO:
# Emulate espn api
# Create "do" function

grid_url <-
  .get_grid_nfl_fl() %>% 
  mutate(path_dl = paste0("data-raw/", str_replace_all(url, "(^.*com\\/)(.*)(\\.shtml.*$)", "\\2"), ".html"))
grid_url

download <- FALSE
if (download) {
  grid_url %>%
    mutate(
      dl =
        purrr::walk2(
          url,
          path_dl,
          ~download.file(
            url = .x,
            destfile = .y,
            quiet = TRUE
          )
        )
    ) %>%
    mutate(dl = if_else(file.exists(path_dl), TRUE, FALSE))
}
# url <- "http://www.footballlocks.com/nfl_lines.shtml"
# path_dl <- "data-raw/nfl_lines.html"
grid_url1 <- grid_url %>% slice(5)
# url <- grid_url1 %>% pull(url)
path_dl <- grid_url1 %>% pull(path_dl)

html <- xml2::read_html(path_dl)
html
# html %>% rvest::html_nodes("table tr td a") %>% rvest::html_text()

fl_raw <-
  html %>%
  .filter_lines_nfl_fl()

# "LA Rams" vs. "LA Chargers" AND ("Chicago", "Arizona"), "Seattle Detroit", "Pittsburgh Tampa Bay"
fl_clean <-
  fl_raw %>%
  .clean_odds_nfl_fl()

fl_odds <-
  fl_clean %>%
  .recode_tm_cols_fl() %>%
  .add_timeperiod_cols_nfl()
fl_odds
fl_odds
fl_odds %>% write_csv(config$path_lines_temp)
