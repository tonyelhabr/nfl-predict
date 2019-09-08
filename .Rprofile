
path_r_profile <- "~/.Rprofile"
if(file.exists(path_r_profile)) {
  source(path_r_profile)
}
rm("path_r_profile")

.library_silently <- function(...) {
  suppressWarnings(suppressPackageStartupMessages(base::library(...)))
}

.library_silently(tidyverse)
.library_silently(rlang)
.library_silently(teplot)
rm('.library_silently')
filter <- dplyr::filter
select <- dplyr::select

invisible(R.utils::sourceDirectory(file.path('R'), recursive = FALSE))
config <- config::get()

if(interactive()) {
  import_nfl_tm()
  import_nfl_game_result()
  import_nba_tm()
  
  # Trying to figure out how to automatically open a script.
  # file.show("R/update-googlesheets-nfl.R")
}

# fs::file_show("R/update-googlesheets-nfl.R")

# Change these options for this project?
# message(tibble:::op.tibble)
# tibble:::tibble_opt("print_max")
