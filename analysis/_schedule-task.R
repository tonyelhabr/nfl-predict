
get_config_schedule <-
  function(dir_script = getwd(),
           file_script,
           index = 1,
           taskname = NULL) {
    if(is.null(taskname)) {
      taskname <- paste0(gsub('[0-9]+-', '', tools::file_path_sans_ext(basename(file_script))), '-', index)
    }
    list(
      taskname = taskname,
      path_rexe = file.path(Sys.getenv('R_HOME'), 'bin', 'Rscript.exe'),
      # path_rexe = 'C:/PROGRA~1/R/R-3.5.2/bin/Rscript.exe',
      path_script = file.path(dir_script, file_script)
    )
  }

config_schedule <- get_config_schedule(file_script = 'analysis/_scrape-odds-tr.R', index = 1)
stopifnot(file.exists(config_schedule$path_script))
stopifnot(file.exists(config_schedule$path_rexe))
config_schedule$taskname
# library('dplyr')
# tasks_existing <- taskscheduleR::taskscheduler_ls() %>% tibble::as_tibble()
# tasknames_existing <- tasks_existing %>% dplyr::distinct(TaskName) %>% dplyr::pull(TaskName)
# stopifnot(!any(config_schedule$taskname == tasknames_existing))

taskscheduleR::taskscheduler_create(
  taskname = config_schedule$taskname,
  rscript = config_schedule$path_script,
  # chedule = 'MINUTE',
  schedule = 'DAILY',
  # modifier = 5,
  Rexe = config_schedule$path_rexe,
  # starttime = format(as.POSIXct('2018-08-02 10:00:00 CDT'), '%H:%M'),
  starttime = format(as.POSIXct(paste0(Sys.Date(), ' 5:00:00 CDT')), '%H:%M'),
  startdate = format(Sys.Date(), '%m/%d/%Y'),
  debug = TRUE
)

