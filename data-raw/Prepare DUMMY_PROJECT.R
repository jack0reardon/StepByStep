DUMMY_PROJECT <- list()

DUMMY_PROJECT$SPECIFICATION <- read_specification_file("./data-raw/DUMMY_PROJECT/Specification.csv")



#### Put it all together ####

DUMMY_PROJECT$STEPS <- list(
  step_1 = get_form_from_specification_file(file_name = "./data-raw/DUMMY_PROJECT/Steps/Step 1 - Analyst Details.csv",
                                            previous_step = STANDARD_LAUNCH_PAGE_STEP_NAME,
                                            next_step = "step_2"),
  step_2 = get_form_from_specification_file(file_name = "./data-raw/DUMMY_PROJECT/Steps/Step 2 - Project Details.csv",
                                            previous_step = "step_1",
                                            next_step = "step_3"),
  step_3 = get_form_from_specification_file(file_name = "./data-raw/DUMMY_PROJECT/Steps/Conclusion.csv",
                                            previous_step = "step_2",
                                            next_step = NULL)
)

usethis::use_data(DUMMY_PROJECT,
                  internal = FALSE,
                  overwrite = TRUE)

