#' calculate species number by plot, tree height class and year
#'
#' This function calculates for each plot, tree height class and year
#' the number of species, total number of regeneration (or interval with mean
#' and confidence interval using a log transformation) and
#' approximate rubbing damage percentage for regeneration.
#' For core area plots, these variables are calculated for each subplot.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, subplot, year, period, height_class,
#' number_of_tree_species, approx_rubbing_damage_perc,
#' mean_number_of_regeneration_ha,
#' lci_number_of_regeneration_ha, uci_number_of_regeneration_ha and
#' approx_nr_regeneration_ha.
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_regeneration <- load_data_regeneration(path_to_fieldmapdb)
#' calculate_regeneration_plot_height(data_regeneration)
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct select summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_height <- function(data_regeneration) {
  check_forrescalc_version_attr(data_regeneration)
  by_plot_height <- data_regeneration %>%
    mutate(
      plotarea_ha = ifelse(.data$plottype == "CA", 0.01, .data$plotarea_ha),
      nr_tmp =
        ifelse(
          !is.na(nr_of_regeneration),
          nr_of_regeneration,
          approx_nr_regeneration
        )
    ) %>%
    group_by(
      .data$plottype, .data$plot_id, .data$subplot_id, .data$period, .data$year,
      .data$height_class, .data$plotarea_ha
    ) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species, na.rm = TRUE),
      approx_nr_regeneration_ha =
        sum(.data$approx_nr_regeneration) / unique(.data$plotarea_ha),
      approx_rubbing_damage_perc = pmin(
        sum(.data$rubbing_damage_number, na.rm = TRUE) * 100 /
          sum(.data$nr_tmp, na.rm = TRUE), 100),
      not_na_rubbing = sum(!is.na(.data$rubbing_damage_number)),
      interval =
        sum_intervals(
          var_min = .data$min_number_of_regeneration,
          var_max = .data$max_number_of_regeneration,
          transformation = "log", na_rm = TRUE
        )
    ) %>%
    ungroup() %>%
    mutate(
      mean_number_of_regeneration_ha = .data$interval$sum / .data$plotarea_ha,
      lci_number_of_regeneration_ha = .data$interval$lci / .data$plotarea_ha,
      uci_number_of_regeneration_ha = .data$interval$uci / .data$plotarea_ha,
      approx_rubbing_damage_perc =
        ifelse(
          .data$not_na_rubbing > 0,
          .data$approx_rubbing_damage_perc,
          NA
        )
    ) %>%
    select(
      -"interval", -"plotarea_ha",
      -"not_na_rubbing"
    )

  attr(by_plot_height, "database") <- attr(data_regeneration, "database")
  attr(by_plot_height, "forrescalc") <- attr(data_regeneration, "forrescalc")

  return(by_plot_height)
}
