#### 1 Prepare ####

rm(list = ls())
devtools::load_all()

source("data-raw/Prepare PACKAGE_DATA.R")
source("data-raw/Prepare DUMMY_PROJECT.R")

usethis::use_package("dplyr")
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
devtools::document()


#### 2 Run ####

rm(list = ls())
devtools::load_all()
run_app()



