app_server <- function(input, output, session) {
  
  #### 1 Reactive values ####
  
  on_load <- shiny::reactiveVal("dummy")
  has_loaded_project <- shiny::reactiveVal(FALSE)
  selected_project <- shiny::reactiveVal(NULL)
  current_attempt <- shiny::reactive(NULL)
  current_step <- shiny::reactiveVal(NULL)
  prior_attempts <- shiny::reactiveVal(get_prior_attempts())
  
  
  #### 2 Reactivity ####
  
  shiny::observeEvent(on_load(), {
    the_project <- DUMMY_PROJECT
  
    first_step <- get_first_step(the_project$STEPS)
    the_project$STEPS[[STANDARD_LAUNCH_PAGE_STEP_NAME]] <- get_standard_launch_page(first_step)
    
    selected_project(the_project)
    
    
    # Create next and previous functionality
    
    lapply(the_project$STEPS,
           function(x) {
             if (!is.null(x$next_step_UI_ID)) {
               shiny::observeEvent(input[[x$next_step_UI_ID]], {
                 current_step(x$next_step)
               }) 
             }
           })
    
    lapply(the_project$STEPS,
           function(x) {
             if (!is.null(x$previous_step_UI_ID)) {
               shiny::observeEvent(input[[x$previous_step_UI_ID]], {
                 current_step(x$previous_step)
               }) 
             }
           })
    
    current_step(STANDARD_LAUNCH_PAGE_STEP_NAME)
    
    has_loaded_project(TRUE)
  })
  
  shiny::observeEvent(has_loaded_project(), {
    if (has_loaded_project()) {
      output[[SPEC_PROJECT_TITLE_UI_ID]] <- shiny::renderText({ selected_project()$SPECIFICATION$project_title })
      output[[VERSION_UI_ID]] <- shiny::renderText({ selected_project()$SPECIFICATION$version })
      output[[SPEC_SHORT_PROJECT_DESCRIPTION_UI_ID]] <- shiny::renderText({ selected_project()$SPECIFICATION$short_project_description })
      
      for (step_number in 1:length(selected_project()$STEPS)) {
        for (server_function in selected_project()$STEPS[[step_number]]$server_functions) {
          server_function(input, output, session, current_attempt, current_step, selected_project)
        }
      }
    }
  })
  
  shiny::observeEvent(current_step(), {
    output[[CURRENT_STEP_NAME_UI_ID]] <- shiny::renderText({ selected_project()$STEPS[[current_step()]]$name})
    output[[CURRENT_STEP_UI_ID]] <- shiny::renderUI({ selected_project()$STEPS[[current_step()]]$UI })
  })
}