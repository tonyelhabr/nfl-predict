
library("tidyverse")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# path_google_token <- "../google_token.rds"
path_data_gdrive <- config$file_db_nfl
path_data_gdrive_backup <-
  sprintf("%s-%s.%s", tools::file_path_sans_ext(path_data_gdrive), timestamp, tools::file_ext(path_data_gdrive))
path_data_dl_temp <-
  sprintf("%s-%s.%s", tools::file_path_sans_ext(path_data_gdrive), "dl", tools::file_ext(path_data_gdrive))

googledrive::drive_auth()
# dribble <- googledrive::drive_get(path_data_gdrive)
drbl <- googledrive::drive_get(path_data_gdrive)
googlesheets::gs_read(drbl, ws = 1)
googlesheets::gs_ls()
library("googlesheets")


# db_nfl <- googlesheets::gs_title(config$file_db_nfl)
db_nfl <- googlesheets::gs_key(config$key_db_nfl) %>% googlesheets::gs_read()
# nfl_game_result <- googlesheets::gs_read(db_nfl, ws = "nfl_game_result")
nfl_tm <- googlesheets::gs_read(db_nfl, ws = "nfl_tm")
nfl_tm
# googledrive::drive_cp(file = path_data_gdrive, path = path_data_gdrive_backup)

# path_data_dl_actual <-
#   googledrive::drive_download(dribble$path, path = path_data_dl_temp, overwrite = TRUE)
# readr::read_rds("../google_token.rds") -> token
# token
# token$credentials
# token$app
# jsonlite::read_json("../google_oath_2.json") -> z
# z
# z %>% saveRDS("../google_token_2.rds")
# drive_auth(oauth_token = "../google_token_2.rds")
# drive_find(type = "spreadsheet")
