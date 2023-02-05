get_form_from_specification_file <- function(file_name, form_name, save_form_UI_ID) {
  DF <- read.csv.default(file_name)
  
  the_form <- list()
  
  the_form$name <- form_name
  
  for (DF_entry in 1:nrow(DF)) {
    the_variable <- DF[DF_entry, "variable"]
    the_data_type <- DF[DF_entry, "data_type"]
    data_type_function <- get_data_type_function(the_data_type)
    the_default_value <- data_type_function(DF[DF_entry, "default_value"])
    the_is_function <- DF[DF_entry, "is_function"]
    default_value_is_a_function <- ifelse(is.na(the_is_function), FALSE, ifelse(nchar(the_is_function) == 0, FALSE, the_is_function == "TRUE"))
    
    if (default_value_is_a_function) {
      the_default_value <- eval(parse(text = the_default_value))
    }
    
    the_min <- data_type_function(DF[DF_entry, "min"])
    the_max <- data_type_function(DF[DF_entry, "max"])
    the_step <- data_type_function(DF[DF_entry, "step"])
    the_choices <- get_choices(the_default_value)
    
    UI_element_id <- get_UI_element_ID(form_name, the_variable)
    
    if (the_data_type == "character") {
      the_element <- shiny::textInput(UI_element_id, label = the_variable, value = the_default_value)
    } else if (the_data_type == "numeric") {
      the_element <- shiny::numericInput(UI_element_id, label = the_variable, value = the_default_value, min = the_min, max = the_max, step = the_step)
    } else if (the_data_type == "date") {
      the_element <- shiny::dateInput(UI_element_id, label = the_variable)
    } else if (the_data_type == "freeform") {
      the_element <- shiny::textAreaInput(UI_element_id, label = the_variable)
    } else if (the_data_type == "slider") {
      the_element <- shiny::sliderInput(UI_element_id, label = the_variable, min = the_min, max = the_max, value = the_default_value, step = the_step)
    } else if (the_data_type == "select") {
      the_element <- shiny::selectInput(UI_element_id, label = the_variable, choices = the_choices, selected = the_choices[1])
    } else if (the_data_type == "checkbox") {
      the_element <- shiny::checkboxGroupInput(UI_element_id, label = the_variable, choices = the_choices, selected = the_choices[1])
    } else if (the_data_type == "radiobutton") {
      the_element <- shiny::radioButtons(UI_element_id, label = the_variable, choices = the_choices, selected = the_choices[1])
    } else {
      stop(paste0("Error: Form ", file_name, " has unrecognised data type ", the_data_type))
    }
    
    the_form$UI[[DF_entry]] <- the_element
    the_form$data_types[[DF_entry]] <- the_data_type
    the_form$variables[[DF_entry]] <- the_variable
  }
  
  the_form$save_function <- get_save_form_server_function(the_form, save_form_UI_ID)
  
  return(the_form)
}

read_specification_file <- function(file_name) {
  listify_DF(read.csv.default(file_name))
}

read.csv.default <- function(file_name, ...) {
  read.csv(file_name, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", sep = ";", ...)
}

write.csv.default <- function(data, file_name, ...) {
  write.csv(data, file_name, row.names = FALSE, quote = FALSE, ...)
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
    as.character(x)
  }
}

get_UI_element_ID <- function(parent_ID, element_ID) {
  return(paste(snakecase::to_snake_case(parent_ID), snakecase::to_snake_case(element_ID), sep = "-"))
}


get_save_form_server_function <- function(form, save_form_UI_ID) {
  # Force evaluation of this dynamic argument the the dynamic function returned
  force(form)
  
  return(function(input, output, session, current_attempt, current_step, selected_project) {
    shiny::observeEvent(input[[save_form_UI_ID]], {
      variables <- sapply(form$variables, function(x) { x })
      
      data_types <- sapply(form$data_types, get_save_data_type_from_form_data_type)
      
      values <- sapply(form$variables,
                       function(variable, form_name) {
                         paste(as.character(input[[get_UI_element_ID(form_name, variable)]]), collapse = CHOICES_SEPARATOR)
                       },
                       form$name)
      
      
      form_data <- data.frame(variable = variables,
                              data_type = data_types,
                              value = values)
      
      if (current_step() == STANDARD_LAUNCH_PAGE_STEP_NAME) {
        attempt_name <- input[[get_UI_element_ID(STANDARD_LAUNCH_PAGE_FORM_NAME, "Attempt Name")]]
      } else {
        attempt_name <- selected_project()$STEPS[[current_step()]]$prior_steps[[STANDARD_LAUNCH_PAGE_STEP_NAME]]$`Attempt Name`
      }
      
      step_directory <- get_step_directory(attempt_name, current_step())
      write.csv.default(form_data, paste0(step_directory, form$name, ".csv"))
    })
  })
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

get_choices <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  stringr::str_split(x, CHOICES_SEPARATOR)[[1]]
}


get_standard_launch_page <- function(next_step) {
  # Force evaluation of this dynamic argument the the dynamic function returned
  force(next_step)
  
  next_step_UI_ID <- get_UI_element_ID(STANDARD_LAUNCH_PAGE_FORM_NAME, "next")
  
  standard_launch_page_form <- get_form_from_specification_file(file_name = "./data-raw/Standard Files/standard_launch_page.csv",
                                                                form_name = STANDARD_LAUNCH_PAGE_FORM_NAME,
                                                                save_form_UI_ID = next_step_UI_ID)
  
  refresh_prior_attempts_AB_ID <- get_UI_element_ID(STANDARD_LAUNCH_PAGE_FORM_NAME, "refresh_prior_attempts")
  
  prior_attempt_UI_ID <- get_UI_element_ID(STANDARD_LAUNCH_PAGE_FORM_NAME, "Load Prior Attempt")

  
  list(
    name = "Load Prior Attempt",
    UI = shiny::mainPanel(
      width = 6,
      standard_launch_page_form$UI,
      shiny::actionButton(refresh_prior_attempts_AB_ID, "Refresh Prior Attempts") ,
      shiny::actionButton(next_step_UI_ID, "Next")
    ),
    server_functions = list(standard_launch_page_form$save_function,
                            function(input, output, session, current_attempt, current_step, selected_project) {
                              shiny::observeEvent(input[[next_step_UI_ID]], {
                                meta_data <- tibble::tribble(
                                  ~variable, ~data_type, ~value,
                                  "date_time", "character", Sys.time()
                                )
                                
                                print(1)
                                
                                attempt_name <- input[[get_UI_element_ID(STANDARD_LAUNCH_PAGE_FORM_NAME, "Attempt Name")]]
                                step_directory <- get_step_directory(attempt_name, current_step())
                                write.csv.default(meta_data, paste0(step_directory, "meta_data.csv"))
                                
                                print(2)
                                
                                # selected_project()$STEPS[[current_step()]]$prior_steps <- list()
                                # selected_project()$STEPS[[current_step()]]$prior_steps[[STANDARD_LAUNCH_PAGE_STEP_NAME]]
                                
                                print(3)
                                
                                current_step(next_step)
                              })
                              
                              shiny::observeEvent(input[[refresh_prior_attempts_AB_ID]], {
                                prior_attempts <- get_prior_attempts()
                                
                                shiny::updateSelectInput(session,
                                                         prior_attempt_UI_ID,
                                                         choices = prior_attempts,
                                                         selected = prior_attempts[1])
                              })
                            })
  )
}


get_prior_attempts <- function() {
  prior_attempts <- list.dirs(path = ATTEMPTS_DIRECTORY, full.names = FALSE, recursive = FALSE)
  
  if (length(prior_attempts) == 0) {
    return("")
  } else {
    return(prior_attempts[order(prior_attempts)]) 
  }
}

get_prior_attempts_for_form <- function() {
  paste(as.character(get_prior_attempts()), collapse = CHOICES_SEPARATOR)
}

get_step_directory <- function(attempt, step) {
  dir.create(paste0(ATTEMPTS_DIRECTORY, "/", attempt), showWarnings = FALSE)
  dir.create(paste0(ATTEMPTS_DIRECTORY, "/", attempt, "/", step), showWarnings = FALSE)
  return(paste0(ATTEMPTS_DIRECTORY, "/", attempt, "/", step, "/"))
}



get_save_data_type_from_form_data_type <- function(form_data_type) {
  if (form_data_type %in% c("character", "date", "freeform", "select", "checkbox", "radiobutton")) {
    return("character")
  } else if (form_data_type %in% c("numeric", "slider")) {
    return("numeric")
  } else {
    return("")
  }
}