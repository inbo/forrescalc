#' Sum up intervals including a confidence interval
#'
#' @description
#' This internal helper function is used to sum up interval data, hereby
#' respecting the confidence interval
#'
#' @param var_min minimum of intervals to be summed up
#' @param var_max maximum of intervals to be summed up
#' @param transformation "log" if log transformation has to be performed on data
#' @param na_rm should NA in `var_min` and `var_max` be ignored?
#'
#' @return dataframe with one record with columns `n_obs`, `sum`, `lci` (lower
#' limit of confidence interval) and `uci` (upper limit of confidence interval)
#'
#' @examples
#' #see code of calc_reg_plot_height()
#'
#' @noRd
#'
sum_intervals <-
  function(var_min, var_max, transformation = NA, na_rm = FALSE) {

  if (na_rm) {
    var_min <- var_min[!is.na(var_min)]
    var_max <- var_max[!is.na(var_max)]
  }
  if (length(var_min) == 0) {
    return(data.frame(n_obs = NA, sum = NA, lci = NA, uci = NA))
  }

  if (!is.na(transformation) && transformation == "log") {
    value <- exp((log(var_min + 1e-10) + log(var_max)) / 2)
    variance <- ((log(var_max) - log(var_min + 1e-10)) / (2 * 1.96)) ^ 2
  } else {
    value <- (var_min + var_max) / 2
    variance <- ((var_max - var_min) / (2 * 1.96)) ^ 2
  }

  result <- data.frame(n_obs = length(value))
  result$sum <-
    ifelse(
      !is.na(transformation) & transformation == "log",
      log(sum(value)),
      sum(value)
    )
  variance <- sum(variance) / result$n_obs
  result$lci <- result$sum - 1.96 * sqrt(variance) / sqrt(result$n_obs)
  result$uci <- result$sum + 1.96 * sqrt(variance) / sqrt(result$n_obs)

  if (!is.na(transformation) && transformation == "log") {
    result$sum <- exp(result$sum)
    result$lci <- exp(result$lci)
    result$uci <-
      ifelse(
        variance == Inf,
        result$sum,
        exp(result$uci)
      )
  }

  return(result)
}
