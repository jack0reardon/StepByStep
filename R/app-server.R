app_server <- function(input, output, session) {
  
  #### 1 Reactive values ####
  
  on_load <- shiny::reactiveVal("dummy")
  has_loaded_project <- shiny::reactiveVal(FALSE)
  project_variables <- shiny::reactiveValues(selected_project = NULL,
                                             current_step = NULL,
                                             current_attempt = NULL,
                                             new_step_has_been_loaded = FALSE,
                                             prior_attempts = get_prior_attempts())
  
  
  #### 2 Reactivity ####
  
  shiny::observeEvent(on_load(), {
    the_project <- DUMMY_PROJECT
    

    # Include "next" and "previous" functionality
    lapply(the_project$STEPS,
           function(x) {
             next_step_was_just_clicked <- get_UI_element_ID(x$name, FORM_NEXT_UI_ID, AB_WAS_JUST_CLICKED)
             shiny::observeEvent(project_variables[[next_step_was_just_clicked]], {
               if (project_variables[[next_step_was_just_clicked]]) {
                 project_variables$current_step <- x$next_step
                 project_variables[[next_step_was_just_clicked]] <- FALSE
               }
             })
           })
    
    lapply(the_project$STEPS,
           function(x) {
             previous_step_was_just_clicked <- get_UI_element_ID(x$name, FORM_PREVIOUS_UI_ID, AB_WAS_JUST_CLICKED)
             shiny::observeEvent(project_variables[[previous_step_was_just_clicked]], {
               if (project_variables[[previous_step_was_just_clicked]]) {
                 project_variables$current_step <- x$previous_step
                 project_variables[[previous_step_was_just_clicked]] <- FALSE
               }
             })
           })
    
    first_step <- get_first_step(the_project$STEPS)
    the_project$STEPS[[STANDARD_LAUNCH_PAGE_STEP_NAME]] <- get_standard_launch_page(first_step)
    
    project_variables$selected_project <- the_project
    
    has_loaded_project(TRUE)
  })
  
  shiny::observeEvent(has_loaded_project(), {
    if (has_loaded_project()) {
      output[[SPEC_PROJECT_TITLE_UI_ID]] <- shiny::renderText({ project_variables$selected_project$SPECIFICATION$project_title })
      output[[VERSION_UI_ID]] <- shiny::renderText({ project_variables$selected_project$SPECIFICATION$version })
      output[[SPEC_SHORT_PROJECT_DESCRIPTION_UI_ID]] <- shiny::renderText({ project_variables$selected_project$SPECIFICATION$short_project_description })
      
      for (step_number in 1:length(project_variables$selected_project$STEPS)) {
        for (server_function in project_variables$selected_project$STEPS[[step_number]]$server_functions) {
          server_function(input, output, session, project_variables)
        }
      }
      
      project_variables$current_step <- STANDARD_LAUNCH_PAGE_STEP_NAME
    }
  })
  
  shiny::observeEvent(project_variables$current_step, {
    output[[CURRENT_STEP_NAME_UI_ID]] <- shiny::renderText({ project_variables$selected_project$STEPS[[project_variables$current_step]]$name})
    output[[CURRENT_STEP_UI_ID]] <- shiny::renderUI({ project_variables$selected_project$STEPS[[project_variables$current_step]]$UI })
    
    project_variables$new_step_has_been_loaded <- TRUE
  })
  
  shiny::observeEvent(project_variables$new_step_has_been_loaded, {
    if (project_variables$new_step_has_been_loaded) {
      # Pre-load the step if applicable
      preload_step(session,
                   current_attempt = project_variables$current_attempt,
                   current_step = project_variables$current_step)
      project_variables$new_step_has_been_loaded <- FALSE
    }
  })
}