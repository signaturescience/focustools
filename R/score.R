#' Function to compute the Weighted Interval Score (WIS)
#'
#' @param forecast A dataframe containing the model-predicted forecasts as obtained from ts_forecast(), load_forecasts(), or similar
#' @param truth A dataframe containing the observed values as obtained from get_data() or similar
#' @param q A vector of quantiles to assess the WIS over
#'
#' @references \url{https://arxiv.org/abs/2005.12881}
#'
#' @return A dataframe containing the weighted interval scores
#'
#' @export

wis <- function(forecast, truth, q = NULL) {

  # Prep the input data frames
  truth <- truth %>%
    dplyr::select(target_variable, target_end_date, location, true_value = value)

  forecast <- forecast %>%
    dplyr::filter(type == "quantile")

  if (!("target_variable" %in% colnames(forecast))) {
    forecast <- forecast %>%
      dplyr::mutate(target_variable = gsub(pattern = "[[:digit:]] wk ahead ", replacement = "", x = target))
  } else {
    forecast <- forecast %>%
      dplyr::mutate(target = paste(horizon, temporal_resolution, "ahead", target_variable))
  }

  if (!is.null(q)) {
    forecast <- forecast %>% dplyr::filter(quantile %in% q)
  }

  # Separate the upper and lower quantile estimates into their own data frames. Note that both data frames include the 0.5 quantile
  lower_quantiles <- forecast[forecast$quantile <= 0.5, ]
  lower_quantiles$alpha <- lower_quantiles$quantile
  lower_quantiles$lower_quantile <- lower_quantiles$value
  lower_quantiels <- subset(lower_quantiles, select = c("forecast_date", "target", "target_end_date", "target_variable", "location", "alpha", "lower_quantile"))

  upper_quantiles <- forecast[forecast$quantile >= 0.5, ]
  upper_quantiles$alpha <- 1 - upper_quantiles$quantile
  upper_quantiles$upper_quantile <- upper_quantiles$value
  upper_quantiles <- subset(upper_quantiles, select = c("forecast_date", "target", "target_end_date", "target_variable", "location", "alpha", "upper_quantile"))

  # Calculate the interval score for each interval.
  interval_scores <- merge(x = lower_quantiles, y = upper_quantiles, by = c("forecast_date", "target", "target_end_date", "target_variable", "location", "alpha"))
  interval_scores <- merge(x = interval_scores, y = truth, by = c("target_variable", "target_end_date", "location"))
  interval_scores$score <- (interval_scores$alpha / 2) * ((interval_scores$upper_quantile - interval_scores$lower_quantile) + ((2 / interval_scores$alpha) * (interval_scores$lower_quantile - interval_scores$true_value) * as.numeric(interval_scores$true_value < interval_scores$lower_quantile)) + ((2 / interval_scores$alpha) * (interval_scores$true_value - interval_scores$upper_quantile) * as.numeric(interval_scores$true_value > interval_scores$upper_quantile)))
  interval_scores$score[interval_scores$alpha == 0.5] <- abs(interval_scores$true_value[interval_scores$alpha == 0.5] - interval_scores$lower_quantile[interval_scores$alpha == 0.5]) * 0.5

  wis <- interval_scores %>%
    dplyr::group_by(target_variable, target_end_date, target, location) %>%
    dplyr::summarise(wis = (1/(dplyr::n() + 0.5)) * sum(score, na.rm = TRUE))

  return(wis)
}
