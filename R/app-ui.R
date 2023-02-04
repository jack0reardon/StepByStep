app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel(shiny::textOutput(SPEC_PROJECT_TITLE_UI)),
    shiny::helpText(shiny::textOutput(SPEC_SHORT_PROJECT_DESCRIPTION_UI)),
    shiny::hr(),
    shiny::sidebarPanel(
      width = 3,
      shiny::div(
        "Hu",
        shiny::hr()
      )
    ),
    shiny::uiOutput(CURRENT_STEP_UI)
  )
}