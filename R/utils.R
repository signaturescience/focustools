#' Make tsibble
#'
#' @param df A tibble containing columns `epiyear` and `epiweek`.
#' @param chop Logical indicating whether or not to remove the most current week (default `TRUE`).
#' @return A tsibble containing additional colums `monday` indicating the date
#'   for the Monday of that epiweek, and `yweek` (a yearweek vctr class object)
#'   that indexes the tsibble in 1 week increments.
#' @export
#' @md
make_tsibble <- function(df, chop=TRUE) {
  out <- df %>%
    # get the monday that starts the MMWRweek
    mutate(monday=MMWRweek::MMWRweek2Date(MMWRyear=epiyear, MMWRweek=epiweek, MMWRday=2), .after="epiweek") %>%
    # convert represent as yearweek (see package?tsibble)
    dplyr::mutate(yweek=tsibble::yearweek(monday), .after="monday") %>%
    # convert to tsibble
    tsibble::as_tsibble(index=yweek)
  # Remove the incomplete week
  if (chop) out <- out %>% dplyr::filter(lubridate::week(monday)!=lubridate::week(lubridate::today()))
  return(out)
}

#' Make quantile tibbles
#'
#' Make a quantile tibble for required quantiles.
#' Defaults to `c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)`.
#' For `N wk ahead inc case target`, filter to: `c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)`
#'
#' @param x A numeric vector.
#' @param q Quantiles to report. Defaults to `c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)`.
#' @return A tibble of quantiles (`q`) and their values (`x`).
#' @export
#' @md
#' @examples
#' quibble(iris$Sepal.Length)
quibble <- function(x, q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) {
  tibble::tibble(q = q, x = stats::quantile(x, q))
}
