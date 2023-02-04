get_form_UI_from_specification_file <- function(file_name, form_name) {
  DF <- read.csv.default(file_name)
  
  the_form <- list()
  
  for (DF_entry in 1:nrow(DF)) {
    the_variable <- DF[DF_entry, "variable"]
    the_data_type <- DF[DF_entry, "data_type"]
    data_type_function <- get_data_type_function(the_data_type)
    the_default_value <- data_type_function(DF[DF_entry, "default_value"])
    the_min <- data_type_function(DF[DF_entry, "min"])
    the_max <- data_type_function(DF[DF_entry, "max"])
    the_step <- data_type_function(DF[DF_entry, "step"])
    
    UI_element_id <- get_UI_element_ID(form_name, snakecase::to_snake_case(the_variable))
    
    if (the_data_type == "character") {
      the_element <- shiny::textInput(
        UI_element_id, label = the_variable, value = the_default_value
      )
    } else if (the_data_type == "numeric") {
      the_element <- shiny::numericInput(
        UI_element_id, label = the_variable, value = the_default_value, min = the_min, max = the_max, step = the_step
      )
    } else {
      the_element <- shiny::helpText("Jack")
    }
    
    the_form[[DF_entry]] <- the_element
  }
  
  the_form[[SUBMIT_FORM_ID]] <- shiny::actionButton(
    get_UI_element_ID(form_name, SUBMIT_FORM_ID), label = "Submit"
  )
  
  return(the_form)
}

read_specification_file <- function(file_name) {
  listify_DF(read.csv.default(file_name))
}

read.csv.default <- function(file_name, ...) {
  read.csv(file_name, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", sep = ";", ...)
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
  } else if (data_type == "factor") {
    as.factor
  } else if (data_type == "numeric") {
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
  return(paste(parent_ID, element_ID, sep = "-"))
}


submit_form <- function(form_name) {
  return(function(input, output, session) {
    list(
      shiny::observeEvent(input[[get_UI_element_ID(form_name, SUBMIT_FORM_ID)]], {
        print(1)
      })
    )
  })
}