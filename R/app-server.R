app_server <- function(input, output, session) {
  
  #### 1 Reactive values ####
  
  on_load <- shiny::reactiveVal("dummy")
  selected_project <- shiny::reactiveVal(NULL)
  main_graph <- shiny::reactiveVal(NULL)
  current_step <- shiny::reactiveVal(1)
  
  
  
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
      output[[SPEC_PROJECT_TITLE_UI]] <- shiny::renderText({ selected_project()$SPECIFICATION$PROJECT_TITLE })
      output[[SPEC_SHORT_PROJECT_DESCRIPTION_UI]] <- shiny::renderText({ selected_project()$SPECIFICATION$SHORT_PROJECT_DESCRIPTION })
      output[[CURRENT_STEP_UI]] <- shiny::renderUI({ selected_project()$STEPS[[current_step()]]$UI })
    }
  })
  
  for (step_number in 1:length(DUMMY_PROJECT$STEPS)) {
    DUMMY_PROJECT$STEPS[[step_number]]$SERVER_FUNCTION(input, output, session)
  }
}