DUMMY_PROJECT <- list()

DUMMY_PROJECT$SPECIFICATION <- read_specification_file("./data-raw/DUMMY_PROJECT/Specification.csv")



#### Step 1 ####

analyst_details_form <- get_form_from_specification_file(file_name = "./data-raw/DUMMY_PROJECT/Steps/Step 1/Analyst Details.csv",
                                                         form_name = "analyst_details",
                                                         save_form_UI_ID = "step_1_to_step_2_AB")


UI_step_1 <- shiny::mainPanel(
  width = 6,
  shiny::helpText("Enter your information here and then click next."),
  analyst_details_form$UI,
  shiny::hr(),
  shiny::actionButton("step_1_to_launch_page_AB", "Previous"),
  shiny::actionButton("step_1_to_step_2_AB", "Next") 
)

server_functions_step_1 <- list(analyst_details_form$server_save_function)


#### Step 2 ####

project_details_form <- get_form_from_specification_file(file_name = "./data-raw/DUMMY_PROJECT/Steps/Step 2/Project Details.csv",
                                                         form_name = "project_details",
                                                         save_form_UI_ID = "save_project_details_AB")

UI_step_2 <- list(project_details_form$UI,
                  shiny::actionButton("step_2_to_step_1_AB", "Previous"),
                  shiny::actionButton("save_project_details_AB", "Save Project Details"))


server_functions_step_2 <- list(project_details_form$server_save_function)





#### Put it all together ####

DUMMY_PROJECT$STEPS <- list(
  step_1 = list(name = "Analyst Details", UI = UI_step_1, server_functions = server_functions_step_1,
                next_step_UI_ID = "step_1_to_step_2_AB", next_step = "step_2",
                previous_step_UI_ID = "step_1_to_launch_page_AB", previous_step = STANDARD_LAUNCH_PAGE_STEP_NAME),
  step_2 = list(name = "Project Details", UI = UI_step_2, server_functions = server_functions_step_2,
                next_UI_ID = NULL, next_step = NULL,
                previous_step_UI_ID = "step_2_to_step_1_AB", previous_step = "step_1")
)

usethis::use_data(DUMMY_PROJECT,
                  internal = FALSE,
                  overwrite = TRUE)

