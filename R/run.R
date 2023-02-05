#' Run the app
#'
#' @export
#' @importFrom dplyr "%>%" filter
run_app <- function() {
  shiny::shinyApp(ui = app_ui(),
                  server = app_server)
}


