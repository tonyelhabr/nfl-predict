
# url <-"blob:https://fontawesome.com/c186a344-3e26-4df9-b265-b36f7556df96"
# path <- file.path("data-raw", "football-ball-solid.svg")
# download.file(url, destfile = path)
# svg <- magick::image_read(path)

#' Reference: https://www.markroepke.me/post/a-quick-guide-to-creating-a-hex-sticker/
#' Downloaded .png file from http://fa2png.io/.
#' - ion-ios-americanfootball-outline
#' - #000000 OR #8b4513.
#' - transparent
#' - 128 px
#' - 0 px
path_import <- 
  list.files(
    path = file.path("data-raw", "logo"),
    pattern = "ionicons_2-0-1_ios-americanfootball_.*png$",
    recursive = FALSE,
    full.names = TRUE
  )
path_import
path_export <- file.path("data", "logo.png")
hexSticker::sticker(
  package = "tenfl",
  p_size = 24,
  p_y = 1.5,
  p_color = "black",
  subplot = path_import,
  s_x = 1,
  s_y = 0.8,
  # s_width = 0.48,
  s_width = 0.42,
  # h_size = 2,
  h_size = 1.5,
  h_color = "black",
  # h_fill = "#196f0c",
  h_fill = "yellow",
  # url = "github.com/tonyelhabr/tenfl",
  # u_color = "white",
  # u_size = 3.5,
  filename = path_export
)
logo <- magick::image_read(path_export)
logo
