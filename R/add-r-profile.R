
debug(
  teproj::add_task_schedule
)
teproj::add_task_schedule()
template_path <- base::system.file("templates", ".Rprofile", package = "teproj")
usethis::use_template(
  template = ".Rprofile",
  package = "teproj"
)
template_path <-
  usethis:::find_template(
    template = ".Rprofile",
    package = "teproj"
  )
usethis:::render_template(
  template = ".Rprofile",
  package = "teproj"
)
template_content_raw <- readLines(template_path, encoding = "UTF-8")
whisker::whisker.render(template_content_raw)
tmpl <- whisker:::parseTemplate(template_content_raw)
tmpl_parse <- tmpl(list())
template_content <- strsplit(tmpl_parse, "\n")[[1]]
new <- usethis:::write_over("here.R", template_content)

data <- list()
partials <- as.environment(list())
debug <- FALSE

usethis::use_template
usethis:::render_template
# ?whisker::whisker.render
whisker::whisker.render
# ?whisker:::parseTemplate
whisker:::parseTemplate

txt <- readLines(template_path, encoding = "UTF-8")
txt
# txt_render <- whisker::whisker.render(txt, data)
# txt_render

tmpl <- whisker:::parseTemplate(txt, partials = partials, debug = debug)
txt_2render <- tmpl(data)

strsplit(txt_2render, "\n")[[1]]

teproj::add_r_profile()
