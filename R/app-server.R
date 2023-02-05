app_server <- function(input, output, session) {
  
  #### 1 Reactive values ####
  
  selected_project <- shiny::reactiveVal(DUMMY_PROJECT)
  current_attempt <- shiny::reactive(NULL)
  current_step <- shiny::reactiveVal(STANDARD_LAUNCH_PAGE_STEP_NAME)
  prior_attempts <- shiny::reactiveVal(get_prior_attempts())
  
  
  #### 2 Reactivity ####
  
  shiny::observeEvent(selected_project(), {
    if (!is.null(selected_project())) {
      output[[SPEC_PROJECT_TITLE_UI_ID]] <- shiny::renderText({ selected_project()$SPECIFICATION$project_title })
      output[[VERSION_UI_ID]] <- shiny::renderText({ selected_project()$SPECIFICATION$version })
      output[[SPEC_SHORT_PROJECT_DESCRIPTION_UI_ID]] <- shiny::renderText({ selected_project()$SPECIFICATION$short_project_description })
    }
  })
  
  shiny::observeEvent(current_step(), {
    output[[CURRENT_STEP_NAME_UI_ID]] <- shiny::renderText({ selected_project()$STEPS[[current_step()]]$name})
    output[[CURRENT_STEP_UI_ID]] <- shiny::renderUI({ selected_project()$STEPS[[current_step()]]$UI })
  })
  
  for (step_number in 1:length(DUMMY_PROJECT$STEPS)) {
    for (server_function in DUMMY_PROJECT$STEPS[[step_number]]$server_functions) {
      server_function(input, output, session, current_attempt, current_step, selected_project)
    }
  }
}