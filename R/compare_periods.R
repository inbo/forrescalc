#' compare parameters between periods (years that parameters are measured)
#'
#' This function compares for each plot (and other provided variables) the differences between periods/years for the column names given in parameter measure_vars. It gives results for differences between subsequent measures (based on 'period') and between the last and the first measure. All column names of the dataset that are not added to parameter measure_vars, are considered as grouping variables, except for period. If the result is not as expected, please verify that the dataset only consists of grouping variables, variables added to measure_vars and period.
#'
#' @param dataset dataframe with values for each period, plot and year, in addition to grouping variables and measure_vars
#' @param measure_vars column names of variables that should be compared between periods (including year)
#'
#' @return dataframe with columns plot, year_diff, n_years, grouping variables and differences between periods for each column of measure_vars
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' library(dplyr)
#' treenr_by_plot <-
#'   read_git(tablename = "dendro_by_plot", repo_path = "C:/gitrepo/forresdat") %>%
#'   select(period, year, plot_id, number_of_tree_species, number_of_trees_ha) %>%
#'   distinct()
#' compare_periods(treenr_by_plot, c("year", "number_of_tree_species", "number_of_trees_ha"))
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows filter group_by group_by_at left_join mutate mutate_at rename_at select ungroup vars
#' @importFrom purrr map
#' @importFrom tidyr nest pivot_longer unnest
#' @importFrom tidyselect starts_with
#' @importFrom rlang .data
#'
compare_periods <- function(dataset, measure_vars) {
  grouping_vars <-
    colnames(dataset)[!colnames(dataset) %in% c(measure_vars, "period")]

  #helper function to replace NA in year due to missing species by year of other
  #measures in the same group (plot_id and period)
  replace_na_year <- function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)

  #helper function that arranges periods into period differences
  diff_fun <- function(data) {
    diff_data <- data.frame(period_max = unique(data$period[data$period != 1])) %>%
      mutate(
        period_min = .data$period_max - 1
      )
    if (max(data$period) > 2) {
      diff_data <- diff_data %>%
      bind_rows(
        data.frame(period_max = max(data$period), period_min = 1)
      )
    }
    diff_results <- diff_data %>%
      mutate(
        period_diff = paste(.data$period_max, "-", .data$period_min)
      ) %>%
      pivot_longer(
        starts_with("period_m"), names_to = "period_order",
        names_prefix = "period_", values_to = "period"
      ) %>%
      left_join(data, by = "period") %>%
      group_by(.data$period_diff) %>%
      nest() %>%
      mutate(
        data = map(data, diff2_fun)
      ) %>%
      unnest()

    return(diff_results)
  }

  #low level helper function that calculates differences
  diff2_fun <- function(data) {
    data_max <- data %>% filter(.data$period_order == "max")
    data_min <- data %>% filter(.data$period_order == "min")
    result <- (data_max[, measure_vars] - data_min[, measure_vars]) %>%
      rename_at(vars(measure_vars), function(x) paste0(x, "_diff")) %>%
      mutate(
        n_years = .data$year_diff,
        year_diff = paste(data_max$year, data_min$year, sep = " - ")
      )
  }

  #main script of compare_periods
  result_diff <- dataset %>%
    group_by(.data$plot_id, .data$period) %>%
    mutate_at(vars(matches("year")), replace_na_year) %>%
    ungroup() %>%
    group_by_at(vars(grouping_vars)) %>%
    nest() %>%
    mutate(
      diff = map(data, diff_fun)
    ) %>%
    select(!!grouping_vars, .data$diff) %>%
    unnest()

  return(result_diff)
}
