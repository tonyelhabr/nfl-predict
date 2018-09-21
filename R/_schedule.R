
get_config_schedule <-
  function(dir_script = getwd(),
           file_script,
           taskname = paste0("_", gsub("[0-9]+-", "", tools::file_path_sans_ext(basename(file_script))))) {
    list(
      taskname = taskname,
      # path_rexe = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
      path_rexe = "C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe",
      path_script = file.path(dir_script, file_script)
    )
  }

config_schedule <- get_config_schedule(file_script = "R/scrape-odds-tr.R")
# config_schedule$taskname <- paste0(config_schedule$taskname, "2")
stopifnot(file.exists(config_schedule$path_script))
stopifnot(file.exists(config_schedule$path_rexe))
# library("dplyr")
# tasks_existing <- taskscheduleR::taskscheduler_ls() %>% tibble::as_tibble()
# tasknames_existing <- tasks_existing %>% dplyr::distinct(TaskName) %>% dplyr::pull(TaskName)
# stopifnot(!any(config_schedule$taskname == tasknames_existing))

taskscheduleR::taskscheduler_create(
  taskname = config_schedule$taskname,
  rscript = config_schedule$path_script,
  # chedule = "MINUTE",
  schedule = "DAILY",
  # modifier = 5,
  Rexe = config_schedule$path_rexe,
  # starttime = format(as.POSIXct("2018-08-02 10:00:00 CDT"), "%H:%M"),
  starttime = format(as.POSIXct(paste0(Sys.Date(), " 5:00:00 CDT")), "%H:%M"),
  startdate = format(Sys.Date(), "%m/%d/%Y"),
  debug = TRUE
)

