#' Check for valid quantile csv file input
#'
#' @param filename Path to the forecast file to be checked
#' @param verbose Boolean indicating whether or not the output from this function should include validation message; default `TRUE`
#' @param install Boolean as to whether or not the `zoltpy` python module should be installed; if `TRUE` the module will be installed to the virtual environment specified in "envname"; default is `FALSE`
#' @param envname Character vector specifying the name of the virtualenv to which the `zoltpy` module should be installed if `install = TRUE`; default is `NULL` which will install the module to a virtualenv named `r-reticulate`
#'
#' @return If `verbose = FALSE`, the returned value will be a boolean with `TRUE` for valid submission file and `FALSE` for invalid file. If `verbose = FALSE`, the function will return a named list with two elements: "valid" (boolean with the `TRUE`/`FALSE` validation code) and "message" (the output from the `zoltpy valid_quantile_csv_file()` function).
#' @export
#'
#' @examples
#' @md
validate_forecast <- function(filename, verbose = TRUE, install = FALSE, envname = NULL) {

  ## uses install method from PyPi
  ## installs to virtualenv (optionally named with envname)
  if(install) {
    if(!is.null(envname)) {
      reticulate::py_install(packages = "zoltpy", envname = envname)
    } else {
      reticulate::py_install(packages = "zoltpy")
    }
  }

  ## load the validate_quantile function into the main py
  reticulate::py_run_string("from zoltpy.covid19 import validate_quantile_csv_file")

  ## use the validate_quantile_csv_file() function imported above
  validate_msg <- reticulate::py$validate_quantile_csv_file(filename)
  ## collapse msg for legibility
  validate_msg <- paste0(validate_msg, collapse = "\n")
  ## check if valid (msg should == 'no errors')
  res <- ifelse(validate_msg == "no errors", TRUE, FALSE)

  ## optionally output 'valid' and 'messsge' as named list
  if(verbose) {
    return(list(valid = res, message = validate_msg))
  } else {
    return(res)
  }
}
