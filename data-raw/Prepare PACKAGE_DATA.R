# A function for the special shiny ID variables
# Their values should be excatly their variable names
create_shiny_ID <- function(ID) {
  do.call("<<-", list(ID, ID))
}


#### Meta information ####

PACKAGE_NAME <- "stepbystep"
PACKAGE_VERSION_NUMBER <- "v1.0"
LOG_FILE_NAME <- "log.csv"
CHOICES_SEPARATOR <- ":"
ATTEMPTS_DIRECTORY <- "./inst/Attempts"
STANDARD_LAUNCH_PAGE_STEP_NAME <- "launch_page"
DO_NOT_LOAD_PRIOR_ATTEMPT <- "N/A"


#### UI Elements ####

# SPEC = "Specification" = Variables that must be defined in the specification file (else it will error)
create_shiny_ID("SPEC_PROJECT_TITLE_UI_ID")
create_shiny_ID("SPEC_SHORT_PROJECT_DESCRIPTION_UI_ID")
create_shiny_ID("CURRENT_STEP_NAME_UI_ID")
create_shiny_ID("CURRENT_STEP_UI_ID")
create_shiny_ID("VERSION_UI_ID")


STANDARD_LAUNCH_PAGE_FORM_NAME <- "standard_launch_page"


usethis::use_data(PACKAGE_NAME,
                  PACKAGE_VERSION_NUMBER,
                  LOG_FILE_NAME,
                  CHOICES_SEPARATOR,
                  ATTEMPTS_DIRECTORY,
                  STANDARD_LAUNCH_PAGE_STEP_NAME,
                  DO_NOT_LOAD_PRIOR_ATTEMPT,
                  
                  SPEC_PROJECT_TITLE_UI_ID,
                  SPEC_SHORT_PROJECT_DESCRIPTION_UI_ID,
                  CURRENT_STEP_NAME_UI_ID,
                  CURRENT_STEP_UI_ID,
                  VERSION_UI_ID,
                  
                  STANDARD_LAUNCH_PAGE_FORM_NAME,
                  
                  internal = TRUE,
                  overwrite = TRUE)
