#' \code{focustools} package
#'
#' Tools for forecasting COVID-19 in the United States
#'
#'
#' @docType package
#' @name focustools
NULL

## quiets concerns of R CMD check re: the non-bound global variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "county",
                                                        "fips",
                                                        "state",
                                                        "epiweek",
                                                        "icases",
                                                        "cases",
                                                        "count",
                                                        "iso2",
                                                        "code3",
                                                        "Country_Region",
                                                        "epiyear",
                                                        "Admin2",
                                                        "FIPS",
                                                        "Province_State",
                                                        "N",
                                                        "quantile",
                                                        ".model",
                                                        ".mean",
                                                        ".sim",
                                                        "days",
                                                        "deaths",
                                                        "forecast_date",
                                                        "ideaths",
                                                        "location",
                                                        "monday",
                                                        "target",
                                                        "target_end_date",
                                                        "target_end_week",
                                                        "x",
                                                        "type",
                                                        "value",
                                                        "yweek"))

