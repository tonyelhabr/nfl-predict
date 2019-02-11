
library("base")
library("methods")
library("datasets")
library("utils")
library("grDevices")
library("graphics")
library("stats")

path_r_profile <- "~/.Rprofile"
if(file.exists(path_r_profile)) {
  source(path_r_profile)
}
rm("path_r_profile")

suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library("tidylog")))
suppressWarnings(suppressPackageStartupMessages(library("rlang")))
suppressWarnings(suppressPackageStartupMessages(library("teplot")))

config <- config::get()

paths_funcs <-
  list.files(
    path = file.path("R", "functions"),
    pattern = "func",
    recursive = FALSE,
    full.names = TRUE
  )
invisible(sapply(paths_funcs, source))
rm("paths_funcs")

if(interactive()) {
  import_nfl_tm()
  import_nfl_game_result()
  import_nba_tm()
  
  # Trying to figure out how to automatically open a script.
  # file.show("R/update-googlesheets-nfl.R")

  # file.show("R/update-googlesheets-nfl.R")

}

# fs::file_show("R/update-googlesheets-nfl.R")

# Change these options for this project?
# message(tibble:::op.tibble)
# tibble:::tibble_opt("print_max")

