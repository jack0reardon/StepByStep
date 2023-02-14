get_form_from_specification_file <- function(file_name,
                                             next_step,
                                             previous_step) {
  DF <- read.csv.default(file_name)
  
  the_form <- list()
  
  the_form$name <- extract_form_name_from_file_name(file_name)
  the_form$next_step <- next_step
  the_form$previous_step <- previous_step
  
  for (DF_entry in 1:nrow(DF)) {
    the_variable <- DF[DF_entry, "variable"]
    the_data_type <- DF[DF_entry, "data_type"]
    data_type_function <- get_data_type_function(the_data_type)
    the_default_value <- data_type_function(DF[DF_entry, "default_value"])
    the_choices <- get_choices(DF[DF_entry, "choices"])
    the_is_function <- DF[DF_entry, "is_function"]
    the_choices_is_a_function <- ifelse(is.na(the_is_function), FALSE, ifelse(nchar(the_is_function) == 0, FALSE, TRUE))
    
    if (the_choices_is_a_function) {
      the_choices <- get_choices(eval(parse(text = the_is_function)))
    }
    
    the_min <- data_type_function(DF[DF_entry, "min"])
    the_max <- data_type_function(DF[DF_entry, "max"])
    the_step <- data_type_function(DF[DF_entry, "step"])

    UI_element_ID <- get_UI_element_ID(the_form$name, the_variable)
    
    the_form$server_functions <- list()
    
    if (the_data_type == "character") {
      the_element <- shiny::textInput(UI_element_ID, label = the_variable, value = the_default_value)
    } else if (the_data_type == "numeric") {
      the_element <- shiny::numericInput(UI_element_ID, label = the_variable, value = the_default_value, min = the_min, max = the_max, step = the_step)
    } else if (the_data_type == "date") {
      the_element <- shiny::dateInput(UI_element_ID, label = the_variable)
    } else if (the_data_type == "freeform") {
      the_element <- shiny::textAreaInput(UI_element_ID, label = the_variable)
    } else if (the_data_type == "slider") {
      the_element <- shiny::sliderInput(UI_element_ID, label = the_variable, min = the_min, max = the_max, value = the_default_value, step = the_step)
    } else if (the_data_type == "select") {
      the_element <- shiny::selectInput(UI_element_ID, label = the_variable, choices = the_choices, selected = the_default_value)
    } else if (the_data_type == "checkbox") {
      the_element <- shiny::checkboxGroupInput(UI_element_ID, label = the_variable, choices = the_choices, selected = the_default_value)
    } else if (the_data_type == "radiobutton") {
      the_element <- shiny::radioButtons(UI_element_ID, label = the_variable, choices = the_choices, selected = the_default_value)
    } else if (the_data_type == "file") {
      the_element <- shiny::fileInput(UI_element_ID, label = the_variable, multiple = TRUE)
    } else if (the_data_type == "help") {
      the_element <- shiny::helpText(the_default_value)
    } else {
      stop(paste0("Error: Form ", file_name, " has unrecognised data type ", the_data_type))
    }
    
    the_form$UI[[DF_entry]] <- the_element
    the_form$data_types[[DF_entry]] <- the_data_type
    the_form$variables[[DF_entry]] <- the_variable
  }
  
  the_form$UI[[length(the_form$UI) + 1]] <- shiny::hr()
  
  if (!is.null(the_form$previous_step)) {
    the_form$UI[[length(the_form$UI) + 1]] <- shiny::actionButton(get_UI_element_ID(the_form$name, FORM_PREVIOUS_UI_ID), "Previous") 
  }
  
  if (!is.null(the_form$next_step)) {
    the_form$UI[[length(the_form$UI) + 1]] <- shiny::actionButton(get_UI_element_ID(the_form$name, FORM_NEXT_UI_ID), "Next")
  }
  
  the_form$server_functions <- append(the_form$server_functions,
                                      get_save_form_server_function(the_form))
  
  return(the_form)
}

read_specification_file <- function(file_name) {
  listify_DF(read.csv.default(file_name))
}

read.csv.default <- function(file_name, ...) {
  read.csv(file_name, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", sep = ";", ...)
}

write.csv.default <- function(data, file_name, ...) {
  write.table(data, file_name, row.names = FALSE, quote = FALSE, sep = ";", ...)
}

listify_DF <- function(DF) {
  the_result <- list()
  
  for (DF_entry in 1:nrow(DF)) {
    the_variable <- DF[DF_entry, "variable"]
    the_data_type <- DF[DF_entry, "data_type"]
    data_type_function <- get_data_type_function(the_data_type)
    the_value <- data_type_function(DF[DF_entry, "value"])
    
    the_result[[the_variable]] <- the_value
  }
  
  return(the_result)
}

get_data_type_function <- function(data_type) {
  if (is.na(data_type)) {
    as_character
  } else if (data_type %in% c("numeric", "slider")) {
    as.numeric
  } else {
    as_character
  }
}

as_character <- function(x) {
  if (is.na(x) | is.null(x)) {
    ""
  } else {
    get_choices(as.character(x))
  }
}

get_UI_element_ID <- function(...) {
  paste(sapply(list(...), snakecase::to_snake_case), collapse = "-")
}


get_save_form_server_function <- function(form) {
  # Force evaluation of these dynamic arguments to the dynamic function returned
  force(form)
  
  list(
    function(input, output, session, project_variables) {
      
      shiny::observeEvent(input[[get_UI_element_ID(form$name, FORM_NEXT_UI_ID)]], {
        variables <- sapply(form$variables, function(x) { x })
        
        data_types <- sapply(form$data_types, function(x) { x })
        
        values <- sapply(form$variables,
                         function(variable, form_name) {
                           paste(as.character(input[[get_UI_element_ID(form_name, variable)]]), collapse = CHOICES_SEPARATOR)
                         },
                         form$name)
        
        
        form_data <- data.frame(variable = variables,
                                data_type = data_types,
                                value = values)
        
        
        if (form$name == STANDARD_LAUNCH_PAGE_FORM_NAME) {
          project_variables$current_attempt <- input[[get_UI_element_ID(form$name, "Attempt Name")]]
        }
        
        step_directory <- get_step_directory(project_variables$current_attempt, project_variables$current_step)
        write.csv.default(form_data, paste0(step_directory, form$name, ".csv"))
        
        project_variables$selected_project$STEPS[[project_variables$current_step]]$cached_data <- form_data
        
        project_variables[[get_UI_element_ID(form$name, FORM_NEXT_UI_ID, AB_WAS_JUST_CLICKED)]] <- TRUE
      })
    },
    
    function(input, output, session, project_variables) {
      shiny::observeEvent(input[[get_UI_element_ID(form$name, FORM_PREVIOUS_UI_ID)]], {
        project_variables[[get_UI_element_ID(form$name, FORM_PREVIOUS_UI_ID, AB_WAS_JUST_CLICKED)]] <- TRUE
      })
    })
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

get_choices <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  stringr::str_split(x, CHOICES_SEPARATOR)[[1]]
}


get_standard_launch_page <- function(next_step) {
  force(next_step)
  
  standard_launch_page_form <- get_form_from_specification_file(file_name = "./data-raw/Standard Files/Load Prior Attempt.csv",
                                                                previous_step = NULL,
                                                                next_step = "step_1")
  
  refresh_prior_attempts_AB_ID <- get_UI_element_ID(STANDARD_LAUNCH_PAGE_FORM_NAME, "refresh_prior_attempts")
  
  prior_attempt_UI_ID <- get_UI_element_ID(STANDARD_LAUNCH_PAGE_FORM_NAME, "Load Prior Attempt")
  # Must match the "Load Prior Attempt" label ("variable" field) of the form, per file:
  # "./data-raw/Standard Files/Load Prior Attempt.csv"
  
  next_step_was_just_clicked <- get_UI_element_ID(STANDARD_LAUNCH_PAGE_FORM_NAME, FORM_NEXT_UI_ID, AB_WAS_JUST_CLICKED)
  
  list(
    name = standard_launch_page_form$name,
    UI = shiny::mainPanel(
      width = 6,
      standard_launch_page_form$UI,
      shiny::actionButton(refresh_prior_attempts_AB_ID, "Refresh Prior Attempts")
    ),
    server_functions = append(standard_launch_page_form$server_functions,
                              list(
                                function(input, output, session, project_variables) {
                                  shiny::observeEvent(project_variables[[next_step_was_just_clicked]], {
                                    if (project_variables[[next_step_was_just_clicked]]) {
                                      meta_data <- tibble::tribble(
                                        ~variable, ~data_type, ~value,
                                        "date_time", "character", Sys.time()
                                      )
                                      
                                      step_directory <- get_step_directory(project_variables$current_attempt, project_variables$current_step)
                                      write.csv.default(meta_data, paste0(step_directory, "meta_data.csv"))
                                      
                                      # Load prior attempt
                                      selected_prior_attempt_name <- input[[prior_attempt_UI_ID]]
                                      if (selected_prior_attempt_name != DO_NOT_LOAD_PRIOR_ATTEMPT) {
                                        load_prior_attempt_steps(selected_prior_attempt_name = selected_prior_attempt_name,
                                                                 current_attempt_name = project_variables$current_attempt,
                                                                 current_attempt_steps = project_variables$selected_project$STEPS)
                                      }
                                      
                                      project_variables$current_step <- next_step
                                      
                                      project_variables[[next_step_was_just_clicked]] <- FALSE
                                    }
                                  })
                                  
                                  
                                },
                                function(input, output, session, project_variables) {
                                  shiny::observeEvent(input[[refresh_prior_attempts_AB_ID]], {
                                    prior_attempts <- get_prior_attempts()

                                    shiny::updateSelectInput(session,
                                                             prior_attempt_UI_ID,
                                                             choices = prior_attempts,
                                                             selected = prior_attempts[1])
                                  })
                                }
                              )),
    previous_step = standard_launch_page_form$previous_step,
    next_step = standard_launch_page_form$next_step
  )
}

get_proposed_attempt_name <- function() {
  return(paste("Attempt", length(list.dirs(path = ATTEMPTS_DIRECTORY, full.names = FALSE, recursive = FALSE)) + 1))
}


get_prior_attempts <- function() {
  prior_attempts <- list.dirs(path = ATTEMPTS_DIRECTORY, full.names = FALSE, recursive = FALSE)
  
  if (length(prior_attempts) > 0) {
    prior_attempts <- prior_attempts[order(prior_attempts)]
  }
  
  return(c(DO_NOT_LOAD_PRIOR_ATTEMPT, prior_attempts))
}

get_prior_attempts_for_form <- function() {
  paste(as.character(get_prior_attempts()), collapse = CHOICES_SEPARATOR)
}

load_prior_attempt_steps <- function(selected_prior_attempt_name,
                                     current_attempt_name,
                                     current_attempt_steps) {
  step_names <- list.dirs(path = paste0(ATTEMPTS_DIRECTORY, "/", selected_prior_attempt_name), full.names = FALSE, recursive = FALSE)
  for (step_name in step_names) {
    if (step_name != STANDARD_LAUNCH_PAGE_STEP_NAME) {
      prior_attempt_step_directory <- get_step_directory(selected_prior_attempt_name, step_name)
      current_attempt_step_directory <- paste0(ATTEMPTS_DIRECTORY, "/", current_attempt_name, "/")
      file.copy(prior_attempt_step_directory, current_attempt_step_directory, recursive = TRUE)
    }
  }
}

get_step_directory <- function(attempt, step) {
  dir.create(paste0(ATTEMPTS_DIRECTORY, "/", attempt), showWarnings = FALSE)
  dir.create(paste0(ATTEMPTS_DIRECTORY, "/", attempt, "/", step), showWarnings = FALSE)
  return(paste0(ATTEMPTS_DIRECTORY, "/", attempt, "/", step, "/"))
}

get_first_step <- function(steps) {
  names(steps)[which(sapply(steps, function(x) { x$previous_step }) == STANDARD_LAUNCH_PAGE_STEP_NAME)]
}


preload_step <- function(session, current_attempt, current_step) {
  # Look for pre-loaded step and update UI elements if found
  if (!is.null(current_attempt)) {
    preloaded_step_files <- list.files(path = paste0(ATTEMPTS_DIRECTORY, "/", current_attempt, "/", current_step, "/"), full.names = TRUE, recursive = FALSE) 

    for (preloaded_step_file in preloaded_step_files) {
      DF <- read.csv.default(preloaded_step_file)
      form_name <- extract_form_name_from_file_name(preloaded_step_file)
      
      for (DF_entry in 1:nrow(DF)) {
        the_variable <- DF[DF_entry, "variable"]
        the_data_type <- DF[DF_entry, "data_type"]
        data_type_function <- get_data_type_function(the_data_type)
        the_value <- data_type_function(DF[DF_entry, "value"])
        
        UI_element_ID <- get_UI_element_ID(form_name, the_variable)
        
        if (the_data_type == "character") {
          shiny::updateTextInput(session, UI_element_ID, value = the_value)
        } else if (the_data_type == "numeric") {
          shiny::updateNumericInput(session, UI_element_ID, value = the_value)
        } else if (the_data_type == "date") {
          shiny::updateDateInput(session, UI_element_ID, value = the_value)
        } else if (the_data_type == "freeform") {
          shiny::updateTextAreaInput(session, UI_element_ID, value = the_value)
        } else if (the_data_type == "slider") {
          shiny::updateSliderInput(session, UI_element_ID, value = the_value)
        } else if (the_data_type == "select") {
          shiny::updateSelectInput(session, UI_element_ID, selected = the_value)
        } else if (the_data_type == "checkbox") {
          shiny::updateCheckboxGroupInput(session, UI_element_ID, selected = the_value)
        } else if (the_data_type == "radiobutton") {
          shiny::updateRadioButtons(session, UI_element_ID, selected = the_value)
        } else if (the_data_type == "file") {
          # Do nothing - fileInput loads files to a temporary location and the information lost each attempt
        } else if (the_data_type == "help") {
          # Do nothing
        } else {
          stop(paste0("Error: Form ", form_name, " has unrecognised data type ", the_data_type))
        }
      }
    }
  }
}

extract_form_name_from_file_name <- function(file_name) {
  tools::file_path_sans_ext(basename(file_name))
}
