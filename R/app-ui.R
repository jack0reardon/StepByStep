app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel(shiny::textOutput(SPEC_PROJECT_TITLE_UI_ID)),
    shiny::h4(shiny::textOutput(SPEC_SHORT_PROJECT_DESCRIPTION_UI_ID)),
    shiny::helpText(shiny::textOutput(VERSION_UI_ID)),
    shiny::hr(),
    shiny::h3(shiny::textOutput(CURRENT_STEP_NAME_UI_ID)),
    shiny::uiOutput(CURRENT_STEP_UI_ID),
  )
}