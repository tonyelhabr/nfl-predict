
# NOTE: See my blog post on colors in team sports.
# (GitHub source: https://github.com/tonyelhabr/colors-eda/blob/master/R/colors-eda-1.Rmd.)
.add_rgb_cols <-
  function(data, col = "rgb_hex") {
    data %>%
      pull(!!sym(col)) %>%
      grDevices::col2rgb() %>%
      t() %>%
      tibble::as_tibble() %>%
      bind_cols(data, .) 
  }

.color_id <-
  function(hex, set = grDevices::colors()) {
    c2 <- grDevices::col2rgb(hex)
    coltab <- grDevices::col2rgb(set)
    cdist <- apply(coltab, 2, function(z) sum((z - c2)^2))
    set[which(cdist == min(cdist))]
  }

.identify_color_name <-
  function(col, set = grDevices::colors()) {
    col %>%
      purrr::map(~.color_id(.x, set)) %>% 
      purrr::map_chr(~.[1]) %>% 
      stringr::str_replace_all("[0-9]", "")
  }

# colors_rnbw_hex <-
#   c(
#     stringr::str_replace_all(grDevices::rainbow(16), "FF$", ""),
#     "#FFFFFF",
#     "#EEEEEE",
#     "#AAAAAA",
#     "#000000"
#   )
# colors_rnbw <- identify_color_name(colors_rnbw_hex)
# colors_rnbw
.colors_excel <- c("yellow", "orange", "blue", "white")
.add_color_nm_col <-
  function(data, col = "rgb_hex", set = .colors_excel, rename = TRUE) {
    res <-
      data %>%
      pull(!!sym(col)) %>%
      .identify_color_name(set = set) %>% 
      tibble::as_tibble() %>% 
      bind_cols(data, .)
    
    if(rename) {
      res <-
        res %>% 
        rename(color_nm = value)
    }
    res
  }

path_odds_tr <-
  file.path("data", "db_nfl.xlsx")

nms_ws <-
  path_odds_tr %>% 
  readxl::excel_sheets()
nms_ws
nm_ws <- "nfl_game_results"
nm_ws %in% nms_ws

ws <-
  path_odds_tr %>%
  readxl::read_excel(sheet = nm_ws)

cells_header <-
  path_odds_tr %>%
  tidyxl::xlsx_cells() %>% 
  filter(row == 1L)

cells_header %>% 
  unnest(character_formatted) %>% 
  # filter(!is.na(color_rgb))
  count(local_format_id, sort = TRUE)
  glimpse()

cells_formats <-
  path_odds_tr %>%
  tidyxl::xlsx_formats()

# NOTE: See the `{tidyxl}` vignette.
cells_formats_rgb <-
  cells_formats$local$fill$patternFill$fgColor$rgb
# cells_header[cells_header$local_format_id  %in% which(!is.na(cells_formats_rgb)), c("sheet", "address", "character")]

# NOTE: Excel colors come in ARGB format, not RGBA!
cells_formats_rgb_df <-
  cells_formats_rgb %>% 
  tibble(rgb_hex = .) %>% 
  mutate(idx = row_number()) %>% 
  filter(!is.na(rgb_hex)) %>% 
  mutate_at(vars(rgb_hex), funs(str_replace_all(., "^FF", ""))) %>% 
  mutate_at(vars(rgb_hex), funs(paste0("#", .)))
cells_formats_rgb_df

cells_header_rgb_hex <-
  cells_header %>%
  left_join(cells_formats_rgb_df, by = c("local_format_id" = "idx")) %>%
  select(sheet, address, row, col, character, local_format_id, rgb_hex) %>%
  # filter(!is.na(rgb_hex)) %>% 
  mutate_at(vars(rgb_hex), funs(coalesce(., "#FFFFFF")))
cells_header_rgb_hex

cells_header_rgb_hex %>% 
  add_rgb_cols() %>% 
  count(red, green, blue, sort = TRUE)

cells_header_rgb_hex %>% 
  add_color_nm_col() %>% 
  count(color_nm, sort = TRUE)

cells_header_rgb_hex %>% 
  add_color_nm_col() %>% 
  filter(color_nm == "blue")
color_nm_df <-
  tribble(
    ~color_nm, ~description,
    "white", "a \"variable\" column (in the \"tidy data\" sense)",
    "orange", "non-source value",
    "blue", "calculation",
    "yellow", "user input",
    "green", "primary key",
    "magenta", "foreign key"
  )
