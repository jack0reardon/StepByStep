DUMMY_PROJECT <- list()

DUMMY_PROJECT$SPECIFICATION <- read_specification_file("./data-raw/DUMMY_PROJECT (Specification).csv")

DUMMY_PROJECT$STEPS <- list(
  list(UI = get_form_UI_from_specification_file(file_name = "./data-raw/DUMMY Steps/Step 1/User Form.csv",
                                                form_name = "my_first_form"),
       SERVER_FUNCTION = submit_form("my_first_form")
       )
)

usethis::use_data(DUMMY_PROJECT,
                  internal = FALSE,
                  overwrite = TRUE)

