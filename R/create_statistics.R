#' Calculate statistics for the given dataset
#'
#' @description
#' This function calculates statistics for the given data (e.g. from the git-repository forresdat) on the specified level (e.g. forest_reserve, period and species) and for the specified variables (e.g. basal_area and volume). Calculated statistics include number of observations, mean, variance and confidence interval (lci and uci).
#'
#' These summary statistics are calculated on the given data, not taking into account absence of observations unless explicitly added as a record with value zero.
#' E.g. if a certain species only occurs in 3 plots out of 10 and no records are added for the 7 remaining plots, the summary statistics (e.g. mean coverage) are calculated on 3 plots.
#' Records with value zero for certain variables (e.g. coverage of a certain species or number of trees for a certain diameter class) can automatically be added using the function add_zeros().
#'
#' @param dataset dataset with data to be summarised with at least columns year and period, e.g. table from git repository forresdat
#' @param level grouping variables that determine on which level the values should be calculated (e.g. forest_reserve, year and species), given as a string or a vector of strings. Defaults to forest_reserve & period.
#' @param variables variable(s) of which summary statistics should be calculated (given as a string or a vector of strings)
#' @param include_year_range Should min_year and max_year be calculated based on a given column year in dataset?  Defaults to FALSE.
#'
#' @return dataframe with the columns chosen for level, a column variable with the chosen variables, and the columns n_obs, mean, variance, lci (lower limit of confidence interval) and uci (upper limit of confidence interval)
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' dendro_by_plot <-
#'   read_forresdat(tablename = "dendro_by_plot", repo_path = "C:/gitrepo/forresdat")
#' create_statistics(
#'   dataset = dendro_by_plot,
#'   level = c("forest_reserve", "period"),
#'   variables = "vol_alive_m3_ha"
#' )
#' dendro_by_diam_plot_species <-
#'   read_forresdat(tablename = "dendro_by_diam_plot_species", repo_path = "C:/gitrepo/forresdat")
#' create_statistics(
#'   dataset = dendro_by_diam_plot_species,
#'   level = c("forest_reserve", "year", "species", "dbh_class_5cm"),
#'   variables = c("basal_area_alive_m2_ha", "basal_area_dead_m2_ha")
#' )
#' vegetation_by_plot <-
#'   read_forresdat(tablename = "vegetation_by_plot", repo_path = "C:/gitrepo/forresdat")
#' create_statistics(dataset = vegetation_by_plot, level = c("forest_reserve", "period"),
#'   variables = c("number_of_species", "cumm_herb_coverage_class_average_perc"))
#' }
#'
#' @export
#'
#' @importFrom assertthat has_name
#' @importFrom dplyr %>% group_by_at left_join mutate select summarise ungroup vars
#' @importFrom tidyselect all_of
#' @importFrom tidyr nest pivot_longer pivot_wider unnest
#' @importFrom rlang .data
#' @importFrom stats var
#'
create_statistics <-
  function(
    dataset, level = c("period", "forest_reserve"), variables,
    include_year_range = FALSE
  ) {

  if (has_name(dataset, "period") & length(unique(dataset$period)) > 1 &
      !"period" %in% c(level, variables)) {
    warning("Are you sure you don't want to include period in level? Your dataset has measurements in different periods.")  #nolint
  }

  for (var in variables) {
    if (!has_name(dataset, var)) {
      assert_that(
        has_name(dataset, paste0("min_", var)) &
                   has_name(dataset, paste0("max_", var)),
        msg = paste0(
          "The variable", var, " or related range variables min_", var,
          "and max_", var, "are not present as column names in the dataset."
        )
      )
      var_min <- paste0("min_", var)
      var_max <- paste0("max_", var)
      dataset <- dataset %>%
        mutate(
          value = (UQ(sym(var_min)) + UQ(sym(var_max))) / 2,
          variance = ((UQ(sym(var_max)) - UQ(sym(var_min))) / (2 * 1.96)) ^ 2
        ) %>%
        nest("{var}" := c(value, variance)) %>%
        select(-UQ(sym(var_min)), -UQ(sym(var_max)))
    } else {
      dataset <- dataset %>%
        mutate(
          value = UQ(sym(var)),
          variance = NA
        ) %>%
        select(-UQ(sym(var))) %>%
        nest("{var}" := c(value, variance))
    }
  }

  statistics <- dataset %>%
    select(all_of(c(level, variables))) %>%
    pivot_longer(cols = all_of(variables), names_to = "variable") %>%
    unnest(cols = value) %>%
    group_by_at(vars(c(level, "variable"))) %>%
    summarise(
      n_obs = n(),
      mean = mean(.data$value),
      variance = mean(.data$variance),
      variance =
        ifelse(is.na(.data$variance), var(.data$value), .data$variance),
      lci = .data$mean - 1.96 * sqrt(.data$variance) / sqrt(n()),
      uci = .data$mean + 1.96 * sqrt(.data$variance) / sqrt(n())
    ) %>%
    ungroup()

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
        warning("No year range is calculated as 'year' is given as a level (so each year has separate records).")
      }
    } else {
      warning("Add column 'year' to dataset if you want the year range to be calculated.")
    }
  }

  return(statistics)
}
