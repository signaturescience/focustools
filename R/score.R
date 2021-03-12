#' Function to compute the Weighted Interval Score (WIS)
#'
#' @param forecast A dataframe containing the model-predicted forecasts
#' @param truth A dataframe containing the observed values
#' @param q A vector of quantiles to assess the WIS over
#'
#' @return A dataframe containing the weighted interval scores
#'
#' @export

wis <- function(forecast, truth, q = NULL) {

  usethis::use_pipe()

  truth <- truth %>%
    dplyr::select(target_variable, target_end_date, location, true_value = value)

  foo <- forecast %>%
    dplyr::filter(type == "quantile") %>%
    dplyr::mutate(target_variable = gsub(pattern = "[[:digit:]] wk ahead ", replacement = "", x = target))

  if (!is.null(q)) {
    foo <- foo %>% dplyr::filter(quantile %in% q)
  }

  foo_lower <- foo[foo$quantile <= 0.5, ]
  foo_lower$alpha <- foo_lower$quantile
  foo_lower$lower <- foo_lower$value
  foo_lower <- subset(foo_lower, select = c("forecast_date", "target", "target_end_date", "target_variable", "location", "alpha", "lower"))

  foo_upper <- foo[foo$quantile >= 0.5, ]
  foo_upper$alpha <- 1 - foo_upper$quantile
  foo_upper$upper <- foo_upper$value
  foo_upper <- subset(foo_upper, select = c("forecast_date", "target", "target_end_date", "target_variable", "location", "alpha", "upper"))

  foo <- merge(x = foo_lower, y = foo_upper, by = c("forecast_date", "target", "target_end_date", "target_variable", "location", "alpha"))
  foo <- merge(x = foo, y = truth, by = c("target_variable", "target_end_date", "location"))
  foo$interval_score <- (foo$alpha / 2) * ((foo$upper - foo$lower) + ((2 / foo$alpha) * (foo$lower - foo$true_value) * as.numeric(foo$true_value < foo$lower)) + ((2 / foo$alpha) * (foo$true_value - foo$upper) * as.numeric(foo$true_value > foo$upper)))
  foo$interval_score[foo$alpha == 0.5] <- abs(foo$true_value[foo$alpha == 0.5] - foo$lower[foo$alpha == 0.5]) * 0.5

  wis <- foo %>%
    dplyr::group_by(target_variable, target_end_date, target, location) %>%
    dplyr::summarise(wis = (1/(n() + 0.5)) * sum(interval_score, na.rm = TRUE))

  return(wis)
}
