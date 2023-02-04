get_form_from_specification_file <- function(file_name, form_name, save_form_UI_ID) {
  DF <- read.csv.default(file_name)
  
  the_form <- list()
  
  the_form$name <- form_name
  
  for (DF_entry in 1:nrow(DF)) {
    the_variable <- DF[DF_entry, "variable"]
    the_data_type <- DF[DF_entry, "data_type"]
    data_type_function <- get_data_type_function(the_data_type)
    the_default_value <- data_type_function(DF[DF_entry, "default_value"])
    the_min <- data_type_function(DF[DF_entry, "min"])
    the_max <- data_type_function(DF[DF_entry, "max"])
    the_step <- data_type_function(DF[DF_entry, "step"])
    
    UI_element_id <- get_UI_element_ID(form_name, the_variable)
    
    if (the_data_type == "character") {
      the_element <- shiny::textInput(
        UI_element_id, label = the_variable, value = the_default_value
      )
    } else if (the_data_type == "numeric") {
      the_element <- shiny::numericInput(
        UI_element_id, label = the_variable, value = the_default_value, min = the_min, max = the_max, step = the_step
      )
    } else if (the_data_type == "date") {
      the_element <- shiny::dateInput(
        UI_element_id, label = the_variable
      )
    } else {
      stop(paste0("Error: Form ", file_name, " has unrecognised data type ", the_data_type))
    }
    
    the_form$UI[[DF_entry]] <- the_element
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
  return(paste(snakecase::to_snake_case(parent_ID), snakecase::to_snake_case(element_ID), sep = "-"))
}


get_save_form_server_function <- function(form, save_form_UI_ID) {
  # Force evaluation of this dynamic argument the the dynamic function returned
  force(form)
  
  return(function(input, output, session) {
    shiny::observeEvent(input[[save_form_UI_ID]], {
      variables <- sapply(form$variables, function(x) { x })
      
      values <- sapply(form$variables,
                       function(variable, form_name) {
                         as.character(input[[get_UI_element_ID(form_name, variable)]])
                       },
                       form$name)
      
      
      form_data <- data.frame(variable = variables, value = values)
      
      write.csv(form_data,
                paste0(form$name, ".csv"),
                row.names = FALSE,
                quote = FALSE)
    })
  })
}