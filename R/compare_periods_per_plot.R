#' compare parameters between periods (years that parameters are measured)
#'
#' This function compares for each plot (and other provided variables) the
#' differences between periods/years for the column names given in parameter
#' measure_vars. It gives results for differences between subsequent measures
#' (based on 'period') and between the last and the first measure.
#' All column names of the dataset that are not added to parameter measure_vars,
#' are considered as grouping variables, except for period. If the result is not
#' as expected, please verify that the dataset only consists of grouping
#' variables, variables added to measure_vars and period.
#'
#' @param dataset dataframe with values for each period, plot and year,
#' in addition to grouping variables and measure_vars
#' @param measure_vars column names of variables that should be compared
#' between periods (including year)
#' @param replace_na_in_vars column names of variables (measure_vars)
#' in which NA should be replaced by 0 in case at least one record is measured
#' in the same plot_id in the same period.
#' Similar to function `add_zeros()`, this argument allows to add zeros to
#' end up with records for all species or heights in which measures were taken,
#' even if this specific species or height was absent in the period (and thus
#' the tree volume or number of trees being 0 for all periods and plots
#' in which observations were done).
#' Care should be taken to use this argument: it should only be used for
#' aggregated results of measures (e.d. tree volume, tree number,...) and
#' not for measures of individual trees (which will lead to unwanted additional
#' records) or id's referring to coded tables.
#' It is useless to use this argument if only one result per plot is given.
#'
#' @return dataframe with columns plot, year_diff, n_years, grouping variables
#' and differences between periods for each column of measure_vars
#'
#' @examples
#' library(forrescalc)
#' library(dplyr)
#' treenr_by_plot <-
#'   read_forresdat_table(tablename = "dendro_by_plot") %>%
#'   select(
#'     period, year, plot_id, number_of_tree_species, number_of_trees_ha
#'   ) %>%
#'   distinct()
#' compare_periods_per_plot(
#'   treenr_by_plot, c("year", "number_of_tree_species", "number_of_trees_ha")
#' )
#'
#' @export
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% all_vars any_vars bind_rows filter filter_at
#' @importFrom dplyr group_by group_by_at inner_join
#' @importFrom dplyr mutate mutate_at select ungroup vars
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect all_of matches
#' @importFrom rlang .data
#'
compare_periods_per_plot <-
  function(dataset, measure_vars, replace_na_in_vars = NA) {
  assert_that(
    all(c("period", "plot_id") %in% names(dataset)),
    msg = "Dataset must contain the columns period and plot_id."
  )
  assert_that(
    all(has_name(dataset, measure_vars)),
    msg =
      "Not all variables mentioned in measure_vars, are present in the dataset"
  )
  if (all(!is.na(replace_na_in_vars)) &&
        !all(replace_na_in_vars %in% measure_vars)) {
    warning(
      "Not all variable names given in replace_na_in_vars are measure_vars. NA is not replaced by 0 in the variables that are not measure_vars." #nolint: line_length_linter
    )
  }
  if (length(replace_na_in_vars) == 1 && is.na(replace_na_in_vars) &&
        !has_name(dataset, "unused_var")) {
    replace_na_in_vars <- "unused_var"
  }
  fill_vars <- rep(list(0), length(measure_vars) - ("year" %in% measure_vars))
  names(fill_vars) <- measure_vars[measure_vars != "year"]
  grouping_vars <-
    colnames(dataset)[!colnames(dataset) %in% c(measure_vars, "period")]
  prefix <- ifelse(length(measure_vars) == 1, paste0(measure_vars, "_"), "")

  #helper function to replace NA in year due to missing species by year of other
  #measures in the same group (plot_id and period)
  replace_na_year <- function(x) {
    x <- ifelse(x == -999999999, NA, x)
    ifelse(is.na(x) & "year" %in% measure_vars, mean(x, na.rm = TRUE), x)
  }
  #helper function to replace NA by 0 in grouping vars
  replace_na_var <- function(x, by) {
    ifelse(
      x == -999999999,
      ifelse(!all(x == -999999999), by, NA),
      x
    )
  }

  dataset_wide <- dataset %>%
    pivot_wider(
      names_from = "period",
      names_prefix = prefix,
      values_from = all_of(measure_vars),
      values_fill = -999999999
    ) %>%
    group_by(.data$plot_id) %>%
    mutate_at(vars(matches("year")), replace_na_year) %>%
    mutate_at(
      vars(!matches(grouping_vars) & matches(replace_na_in_vars)),
      replace_na_var, by = 0
    ) %>%
    mutate_at(vars(!matches(grouping_vars)), replace_na_var, by = NA) %>%
    ungroup()
  dataset_long <- dataset_wide %>%
    filter_at(vars(all_of(grouping_vars)), any_vars(is.na(.))) %>%
    filter_at(vars(!matches(c(grouping_vars, "year")))
              , all_vars(is.na(.) | . == 0)) %>%
    anti_join(x = dataset_wide, by = grouping_vars) %>%
    pivot_longer(
      cols = matches(measure_vars),
      names_to = "measure_var_period"
    ) %>%
    mutate(
      measure_var =
        substr(.data$measure_var_period, 1
               , nchar(.data$measure_var_period) - 2),
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
    filter(!is.na(.data$value))

  dataset_grouped <- dataset_long %>%
    filter(.data$period == 1) %>%
    inner_join(
      dataset_long %>%
        filter(.data$period == 2),
      by = c(grouping_vars, "measure_var"),
      suffix = c("_1", "_2")
    )
  if (max(dataset_long$period) > 2) {
    for (i in 3:max(dataset_long$period)) {
      dataset_grouped <- dataset_grouped %>%
        bind_rows(
          dataset_long %>%
            filter(.data$period == i - 1) %>%
            inner_join(
              dataset_long %>%
                filter(.data$period == i),
              by = c(grouping_vars, "measure_var"),
              suffix = c("_1", "_2")
            )
        )
    }
  }

  if (0 %in% dataset_long$period) {
    dataset_grouped <- dataset_grouped %>%
      bind_rows(
        dataset_long %>%
          filter(.data$period == 0) %>%
          inner_join(
            dataset_long %>%
              filter(.data$period == 1),
            by = c(grouping_vars, "measure_var"),
            suffix = c("_1", "_2")
          )
      )
  }

  result_diff <- dataset_grouped %>%
    mutate(
      diff_2_1 = .data$value_2 - .data$value_1,
      year_diff =
        ifelse(
          .data$measure_var == "n_year",
          paste(.data$value_1, .data$value_2, sep = " - "),
          ""
        ),
      value_1 = NULL, value_2 = NULL,
      period_diff = paste(.data$period_1, .data$period_2, sep = "_")
    ) %>%
    group_by_at(vars(all_of(grouping_vars), "period_1")) %>%
    mutate(
      year_diff = paste(.data$year_diff, collapse = "")
    ) %>%
    ungroup() %>%
    select(-"period_1", -"period_2") %>%
    pivot_wider(
      names_from = "measure_var",
      values_from = "diff_2_1"
    )
  if (!"year" %in% measure_vars) {
    result_diff <- result_diff %>%
      select(-"year_diff")
  }

  return(result_diff)
}
