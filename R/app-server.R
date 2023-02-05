app_server <- function(input, output, session) {
  
  #### 1 Reactive values ####
  
  on_load <- shiny::reactiveVal("dummy")
  selected_project <- shiny::reactiveVal(NULL)
  main_graph <- shiny::reactiveVal(NULL)
  current_step <- shiny::reactiveVal(1)
  prior_attempts <- shiny::reactiveVal(get_prior_attempts())
  
  
  
  #### 1.1 First observe: Load data ####
  
  shiny::observeEvent(on_load(), {
    waiter::waiter_show(html = waiter::spin_wave())
    selected_project(DUMMY_PROJECT)
  })

  shiny::observeEvent(selected_project(), {
    waiter::waiter_hide()
  })
  
  shiny::observeEvent(selected_project(), {
    if (!is.null(selected_project())) {
      output[[SPEC_PROJECT_TITLE_UI_ID]] <- shiny::renderText({ selected_project()$SPECIFICATION$project_title })
      output[[VERSION_UI_ID]] <- shiny::renderText({ selected_project()$SPECIFICATION$version })
      output[[SPEC_SHORT_PROJECT_DESCRIPTION_UI_ID]] <- shiny::renderText({ selected_project()$SPECIFICATION$short_project_description })
      output[[CURRENT_STEP_NAME_UI_ID]] <- shiny::renderText({ selected_project()$STEPS[[current_step()]]$name})
      output[[CURRENT_STEP_UI_ID]] <- shiny::renderUI({ selected_project()$STEPS[[current_step()]]$UI })
    }
  })
  
  for (step_number in 1:length(DUMMY_PROJECT$STEPS)) {
    for (server_function in DUMMY_PROJECT$STEPS[[step_number]]$server_functions) {
      server_function(input, output, session, current_step, prior_attempts)
    }
  }
}