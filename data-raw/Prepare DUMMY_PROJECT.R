DUMMY_PROJECT <- list()

DUMMY_PROJECT$SPECIFICATION <- read_specification_file("./data-raw/DUMMY_PROJECT (Specification).csv")



#### UI ####

save_analyst_details_AB <- shiny::actionButton("save_analyst_details_AB", "Save Analyst Details")

analyst_details_form <- get_form_from_specification_file(file_name = "./data-raw/DUMMY Steps/Step 1/Analyst Details.csv",
                                                         form_name = "analyst_details",
                                                         save_form_UI_ID = "save_analyst_details_AB")


save_project_details_AB <- shiny::actionButton("save_project_details_AB", "Save Project Details")

project_details_form <- get_form_from_specification_file(file_name = "./data-raw/DUMMY Steps/Step 1/Project Details.csv",
                                                         form_name = "project_details",
                                                         save_form_UI_ID = "save_project_details_AB")


next_step_AB <- shiny::actionButton("step_1_to_step_2_AB", "Next")




#### Steps ####



#### UI and Server Functions ####

UI_step_1 <- shiny::mainPanel(
  width = 6,
  analyst_details_form$UI, save_analyst_details_AB,
  shiny::hr(),
  next_step_AB 
)

UI_step_2 <- list(project_details_form$UI, save_project_details_AB)


server_functions_step_1 <- list(analyst_details_form$save_function,
                                function(input, output, server) {
                                  shiny::observeEvent(input[["step_1_to_step_2_AB"]], {
                                    print(3)
                                    print(environment())
                                    print(parent.env(environment()))
                                    print(parent.env(parent.env(environment())))
                                    print(parent.env(parent.env(parent.env(environment()))))
                                    print(4)
                                    # current_step(2)
                                  })
                                })

server_functions_step_2 <- list(project_details_form$save_function)


DUMMY_PROJECT$STEPS <- list(
  list(UI = UI_step_1, SERVER_FUNCTIONS = server_functions_step_1),
  list(UI = UI_step_2, SERVER_FUNCTIONS = server_functions_step_2)
)

usethis::use_data(DUMMY_PROJECT,
                  internal = FALSE,
                  overwrite = TRUE)

