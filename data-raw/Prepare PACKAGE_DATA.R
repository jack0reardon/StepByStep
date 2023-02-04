# A function for the special shiny ID variables
# Their values should be excatly their variable names
create_shiny_ID <- function(ID) {
  do.call("<<-", list(ID, ID))
}


#### Meta information ####

PACKAGE_NAME <- "stepbystep"
VERSION_NUMBER <- "v1.0"
LOG_FILE_NAME <- "log.csv"


#### UI Elements ####

# SPEC = "Specification" = Variables that must be defined in the specification file (else it will error)
create_shiny_ID("SPEC_PROJECT_TITLE_UI")
create_shiny_ID("SPEC_SHORT_PROJECT_DESCRIPTION_UI")
create_shiny_ID("CURRENT_STEP_UI")
create_shiny_ID("SUBMIT_FORM_ID")



usethis::use_data(PACKAGE_NAME,
                  VERSION_NUMBER,
                  LOG_FILE_NAME,
                  
                  SPEC_PROJECT_TITLE_UI,
                  SPEC_SHORT_PROJECT_DESCRIPTION_UI,
                  CURRENT_STEP_UI,
                  SUBMIT_FORM_ID,
                  
                  internal = TRUE,
                  overwrite = TRUE)
