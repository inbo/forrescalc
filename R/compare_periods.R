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
#' treenr_by_plot <-
#'   read_git(tablename = "dendro_by_plot", repo_path = "C:/gitrepo/forresdat") %>%
#'   select(period, year, plot_id, number_of_tree_species, number_of_trees_ha) %>%
#'   distinct()
#' calculate_dendro_plot_diff(treenr_by_plot, c("year", "number_of_tree_species", "number_of_trees_ha"))
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% mutate select transmute
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'
compare_periods <- function(dataset, measure_vars) {
  fill_vars <- rep(list(0), length(measure_vars) - 1)
  names(fill_vars) <- measure_vars[measure_vars != "year"]
  grouping_vars <-
    colnames(dataset)[!colnames(dataset) %in% c(measure_vars, "period")]

  replace_na_year <- function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)

  by_plot_diff <- dataset %>%
    pivot_wider(
      names_from = "period",
      values_from = measure_vars,
      values_fill = fill_vars
    ) %>%
    group_by(.data$plot_id) %>%
    mutate_at(vars(matches("year")), replace_na_year) %>%
    ungroup() %>%
    transmute(  #calculate: make the comparison
      #grouping_vars (waardes overnemen uit voorgaande df)
      period_diff = "2 - 1",
      year_diff = paste(.data$year_2, .data$year_1, sep = " - "),
      n_years = .data$year_2 - .data$year_1,
      #measure_vars (telkens 2 - 1 doen, of eigenlijk alle opeenvolgende periodes en laatste - eerste)
    )

  return(by_plot_diff)
}
