#' Dummy data used by AvE
#'
#' A list of 1) A data.frame containing actual data, 2) An R object holding the
#' model that attempts to predict the actual data, 3) The field name that is
#' being predicted by the model, 4) A function that applies the GLM to data
#' 
#' \describe{
#'   \item{DATA}{The actual data}
#'   \item{MODEL}{The model that predicts}
#'   \item{RESPONSE}{The name of the field that is predicted by MODEL}
#'   \item{PREDICT_fUNCTION}{A function which takes as input MODEL and a data.frame
#'    and returns the result of the model applied to the data, typically by use
#'    of predict()}
#' }
#' @source /data-raw/Prepare DUMMY_PROJECT.R
"DUMMY_PROJECT"