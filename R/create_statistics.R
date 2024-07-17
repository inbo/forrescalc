#' Calculate statistics for the given dataset
#'
#' @description
#' This function calculates statistics for the given data
#' (e.g. from the git-repository `forresdat`) on the specified level
#' (e.g. forest_reserve, period and species) and for the specified variables
#' (e.g. basal_area and volume).
#' Calculated statistics include number of observations, mean, variance
#' and confidence interval with lower and upper limit (lci and uci).
#'
#' These summary statistics are calculated on the given data, not taking into
#' account absence of observations unless explicitly added as a record with
#' value zero.
#' E.g. if a certain species only occurs in 3 plots out of 10 and no records are
#' added for the 7 remaining plots, the summary statistics (e.g. mean coverage)
#' are calculated on 3 plots.
#' Records with value zero for certain variables (e.g. coverage of a certain
#' species or number of trees for a certain diameter class) can automatically
#' be added using the function add_zeros().
#'
#' In case of intervals, the variance and confidence interval are calculated
#' based on the minimum and maximum values of the intervals of the individual
#' records (which is considered a CI, so lci and uci can serve as min and max).
#' For this, `dataset` must contain columns with minimum and maximum values,
#' `variables` must contain a name for the output of this variable, and
#' `interval_information` must contain the variable names for minimum, maximum
#' and output that should be used.
#' In `interval_information` it can be specified if a logarithmic transformation
#' is needed to compensate of unequal interval widths.
#' In this case, mean and the confidence interval are transformed back,
#' but variance is not, as this result would be confusing rather than useful.
#' For typical `forresdat` variables,
#' the default value of `interval_information`
#' can be used and in this case, the variable mentioned in `variables` should
#' be named after the values in `forresdat`, omitting `min_`, `_min`, `max_` or
#' `_max` (see example on interval data).
#'
#' @param dataset dataset with data to be summarised with at least columns year
#' and period, e.g. table from git repository `forresdat`
#' @param level grouping variables that determine on which level the values
#' should be calculated (e.g. forest_reserve, year and species), given as a
#' string or a vector of strings. Defaults to forest_reserve & period.
#' @param variables variable(s) of which summary statistics should be
#' calculated (given as a string or a vector of strings)
#' @param include_year_range Should min_year and max_year be calculated based
#' on a given column year in dataset?  Defaults to FALSE.
#' @param na_rm Should NA values in the dataset be ignored?  Defaults to FALSE.
#' If TRUE, levels without any non NA data are kept (resulting in NA values).
#' @param interval_information overview of names for interval data,
#' including columns `var_name` (= name for output), `var_min` and `var_max`
#' (= names for minimum and maximum value in input dataset), and
#' `preferred_transformation` (= "log" if log-transformation is desired).
#' Defaults to a table containing all interval variables in `forresdat`,
#' where log transformation is applied in variables where class widths differ.
#' (In cover data in the Longo scale, log transformation is only applied in
#' variables where most observations have a low coverage, e.g. moss cover,
#' in congruence with the fact that class widths only differ in the lower part
#' of the Longo scale.)
#'
#' @return dataframe with the columns chosen for level, a column variable with
#' the chosen variables, and the columns `n_obs`, `mean`, `variance`,
#' `lci` (lower limit of confidence interval) and
#' `uci` (upper limit of confidence interval)
#'
#' @examples
#' library(forrescalc)
#' dendro_by_plot <- read_forresdat_table(tablename = "dendro_by_plot")
#' create_statistics(
#'   dataset = dendro_by_plot,
#'   level = c("forest_reserve", "period"),
#'   variables = "vol_alive_m3_ha"
#' )
#' dendro_by_diam_plot_species <-
#'   read_forresdat_table(tablename = "dendro_by_diam_plot_species")
#' create_statistics(
#'   dataset = dendro_by_diam_plot_species,
#'   level = c("forest_reserve", "year", "species", "dbh_class_5cm"),
#'   variables = c("basal_area_alive_m2_ha", "basal_area_dead_m2_ha")
#' )
#' #example on interval data (shrub_cover and tree_cover)
#' veg_by_plot <- read_forresdat_table(tablename = "veg_by_plot")
#' create_statistics(dataset = veg_by_plot,
#'   level = c("forest_reserve", "period", "plottype"),
#'   variables = c("number_of_species", "shrub_cover", "tree_cover")
#' )
#' # example on data with confidence interval (number_established_ha and
#' # number_seedlings_ha)
#' reg_by_plot <-
#'   read_forresdat_table(tablename = "reg_by_plot")
#' create_statistics(dataset = reg_by_plot,
#'   level = c("forest_reserve", "period", "plot_id"),
#'   variables = c("number_established_ha", "number_seedlings_ha")
#' )
#'
#' @export
#'
#' @importFrom assertthat has_name
#' @importFrom dplyr %>% distinct filter group_by_at left_join mutate select
#' @importFrom dplyr summarise right_join ungroup vars
#' @importFrom tidyselect all_of
#' @importFrom tidyr nest pivot_longer pivot_wider unnest
#' @importFrom readr read_csv2
#' @importFrom rlang .data := sym
#' @importFrom stats var
#'
create_statistics <-
  function(
    dataset, level = c("period", "forest_reserve"), variables,
    include_year_range = FALSE, na_rm = FALSE,
    interval_information =
      suppressMessages(
        read_csv2(system.file("extdata/class_data.csv", package = "forrescalc"))
      )
  ) {

  if (has_name(dataset, "period") && length(unique(dataset$period)) > 1 &&
      !"period" %in% c(level, variables)) {
    warning(
      "Are you sure you don't want to include period in level? Your dataset has measurements in different periods." #nolint: line_length_linter
    )
  }

  assert_that(
    all(has_name(dataset, level)),
    msg = "Dataset should contain all columns that are mentioned as level."
  )

  for (variab in variables) {
    if (!has_name(dataset, variab)) {
      assert_that(is.data.frame(interval_information))
      assert_that(
        has_name(
          interval_information,
          c("var_name", "var_min", "var_max", "preferred_transformation")
        )
      )
      var_info <- interval_information %>%
        filter(.data$var_name == variab) %>%
        distinct()
      assert_that(
        nrow(var_info) > 0,
        msg = paste0(
          "The variable '", variab, "' is not present in the given dataset ",
          "and not declared in 'interval_information'.",
        )
      )
      assert_that(
        nrow(var_info) == 1,
        msg = paste0(
          "The variable '", variab, "' is declared more than once in ",
          "'interval_information' with different parameters. ",
          "Please add each 'var_name' only once in 'interval_information'."
        )
      )
      var_min <- sym(var_info$var_min)
      var_max <- sym(var_info$var_max)
      dataset <- dataset %>%
        mutate(
          logaritmic = !is.na(var_info$preferred_transformation) &
            var_info$preferred_transformation == "log",
          value =
            ifelse(
              .data$logaritmic,
              (log(!!var_min + 1e-10) + log(!!var_max)) / 2,
              (!!var_min + !!var_max) / 2
            ),
          variance =
            ifelse(
              .data$logaritmic,
              ((log(!!var_max) - log(!!var_min + 1e-10)) / (2 * 1.96)) ^ 2,
              ((!!var_max - !!var_min) / (2 * 1.96)) ^ 2
            )
        ) %>%
        nest("{variab}" := c(.data$value, .data$variance, .data$logaritmic)) %>%  # nolint: object_name_linter
        select(-!!var_min, -!!var_max)
    } else {
      dataset <- dataset %>%
        mutate(
          logaritmic = FALSE,
          value = !!sym(variab),
          variance = NA
        ) %>%
        select(-!!sym(variab)) %>%
        nest("{variab}" := c(.data$value, .data$variance, .data$logaritmic))
    }
  }

  dataset <- dataset %>%
    select(
      all_of(level), all_of(variables),
      "year"[include_year_range & has_name(dataset, "year")]
    ) %>%
    pivot_longer(cols = all_of(variables), names_to = "variable") %>%
    unnest(cols = .data$value)
  if (na_rm) {
    dataset <- dataset %>%
      filter(!is.na(.data$value)) %>%
      right_join(
        dataset %>%
          select(all_of(level), "variable", "logaritmic") %>%
          distinct(),
        by = c(level, "variable", "logaritmic")
      )
  }

  statistics <- dataset %>%
    group_by_at(vars(c(level, "variable"))) %>%
    summarise(
      n_obs = n(),
      mean = mean(.data$value),
      variance = sum(.data$variance) / .data$n_obs,
      variance =
        ifelse(is.na(.data$variance), var(.data$value), .data$variance),
      lci = .data$mean - 1.96 * sqrt(.data$variance) / sqrt(n()),
      uci = .data$mean + 1.96 * sqrt(.data$variance) / sqrt(n()),
      logaritmic = unique(.data$logaritmic)
    ) %>%
    ungroup() %>%
    mutate(
      mean = ifelse(.data$logaritmic, exp(.data$mean), .data$mean),
      lci = ifelse(.data$logaritmic, exp(.data$lci), .data$lci),
      uci = ifelse(.data$logaritmic, exp(.data$uci), .data$uci)
    )

  if (include_year_range) {
    if (has_name(dataset, "year")) {
      if (!"year" %in% level) {
        statistics <- dataset %>%
          group_by_at(vars(level)) %>%
          summarise(
            min_year = min(.data$year),
            max_year = max(.data$year)
          ) %>%
          ungroup() %>%
          left_join(
            statistics,
            by = level
          )
      } else {
        warning("No year range is calculated as 'year' is given as a level (so each year has separate records).") #nolint
      }
    } else {
      warning("Add column 'year' to dataset if you want the year range to be calculated.") #nolint
    }
  }

  return(statistics)
}
