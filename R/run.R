#' Run the app
#'
#' @export
#' @importFrom dplyr "%>%" filter
run_app <- function() {
  shiny::shinyApp(ui = app_ui(),
                  server = app_server)
}




add_visual_resources <- function() {
  log_action(action_category = "Setup", action = "Adding visual resources", reference = NULL)
  
  lang = getOption("highcharter.lang")
  lang$numericSymbols <- c("k", "M", "B")
  options("highcharter.lang" = lang)
  
  shiny::addResourcePath("www", get_system_file("www", package_name = PACKAGE_NAME))
}

get_system_file <- function(x, package_name) {
  return(system.file(x, package = package_name))
}


errors_to_char <- function(x) {
  ifelse(is.null(x), "NULL", ifelse(is.na(x), "NA", is.character(x)))
}


log_action <- function(action_category,
                       action,
                       reference,
                       do_clear_log = FALSE) {
  log_file_path <- paste0("inst/", LOG_FILE_NAME)
  log_file_already_exists <- nchar(get_system_file(LOG_FILE_NAME, package_name = PACKAGE_NAME)) > 0
  
  data_to_append_to_log <- tibble::tribble(
    ~time, ~action_category, ~action, ~reference,
    format(Sys.time(), "%Y/%m/%d %H:%M:%S"), errors_to_char(action_category), errors_to_char(action), errors_to_char(reference)
  )
  
  data.table::fwrite(x = data_to_append_to_log,
                     file = log_file_path,
                     append = !do_clear_log & log_file_already_exists)
}