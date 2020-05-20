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
#' @importFrom dplyr %>% group_by group_by_at mutate mutate_at select ungroup vars
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect all_of matches
#' @importFrom rlang .data
#'
compare_periods <- function(dataset, measure_vars) {
  if (!all(c("period", "plot_id") %in% names(dataset))) {
    stop("Dataset must contain the columns period and plot_id.")
  }
  fill_vars <- rep(list(0), length(measure_vars) - ("year" %in% measure_vars))
  names(fill_vars) <- measure_vars[measure_vars != "year"]
  grouping_vars <-
    colnames(dataset)[!colnames(dataset) %in% c(measure_vars, "period")]

  #helper function to replace NA in year due to missing species by year of other
  #measures in the same group (plot_id and period)
  replace_na_year <- function(x) {
    ifelse(is.na(x) & "year" %in% measure_vars, mean(x, na.rm = TRUE), x)
  }

  result_diff <- dataset %>%
    pivot_wider(
      names_from = "period",
      values_from = all_of(measure_vars),
      values_fill = fill_vars
    ) %>%
    group_by(.data$plot_id) %>%
    mutate_at(vars(matches("year")), replace_na_year) %>%
    ungroup() %>%
    pivot_longer(
      cols = matches(measure_vars),
      names_to = "measure_var_period"
    ) %>%
    mutate(
      measure_var =
        substr(.data$measure_var_period, 1, nchar(.data$measure_var_period) - 2),
      measure_var =
        ifelse(
          .data$measure_var == "year",
          "n_year",
          paste(.data$measure_var, "diff", sep = "_")
        ),
      period =
        substr(
          .data$measure_var_period,
          nchar(.data$measure_var_period), nchar(.data$measure_var_period)
        ),
      measure_var_period = NULL
    ) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      names_prefix = "p"
    ) %>%
    mutate(
      diff_2_1 = .data$p2 - .data$p1,
      year_diff =
        ifelse(
          .data$measure_var == "n_year",
          paste( .data$p2, .data$p1, sep = " - "),
          ""
        ),
      p1 = NULL, p2 = NULL
    ) %>%
    group_by_at(vars(all_of(grouping_vars))) %>%
    mutate(
      year_diff = paste(.data$year_diff, collapse = "")
    ) %>%
    ungroup() %>%
    pivot_wider(
      names_from = "measure_var",
      values_from = "diff_2_1"
    )
  if (!"year" %in% measure_vars) {
    result_diff <- result_diff %>%
      select(-.data$year_diff)
  }

  return(result_diff)
}
