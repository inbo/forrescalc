#' aggregate regeneration parameters by plot, height, species and year
#'
#' This function calculates for each plot, height, species and year the number
#' of regeneration per ha (or interval with mean and confidence interval
#' using a log transformation), the number and percentage of
#' subplots in which the species is regenerating and
#' the approximate rubbing damage percentage per hectare.
#' This calculation is designed for core areas, that consist of different
#' subplots.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns `plot`, `species`, `year`, `height`,
#' `nr_of_subplots_with_regeneration`, `perc_subplots_with_regeneration`,
#' `approx_rubbing_damage_perc`, `mean_number_of_regeneration_ha`,
#' `lci_number_of_regeneration_ha`, `uci_number_of_regeneration_ha` and
#' `approx_nr_regeneration_ha.`
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_regeneration_CA <-
#'   load_data_regeneration(path_to_fieldmapdb, plottype = "CA")
#' calc_reg_core_area_height_spec(data_regeneration_CA)
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct select summarise ungroup
#' @importFrom rlang .data
#'
calc_reg_core_area_height_spec <- function(data_regeneration) {
  check_forrescalc_version_attr(data_regeneration)
  by_plot_species <- data_regeneration %>%
    mutate(
      nr_tmp =
        ifelse(
          !is.na(.data$nr_of_regeneration),
          .data$nr_of_regeneration,
          .data$approx_nr_regeneration
        )
    ) %>%
    group_by(.data$plottype, .data$plot_id, .data$period) %>%
    mutate(
      n_subplots = n_distinct(.data$subplot_id)
    ) %>%
    ungroup() %>%
    group_by(
      .data$plottype, .data$plot_id, .data$period, .data$year,
      .data$height_class, .data$species, .data$plotarea_ha
    ) %>%
    summarise(
      nr_of_subplots_with_regeneration = n_distinct(.data$subplot_id),
      perc_subplots_with_regeneration =
        .data$nr_of_subplots_with_regeneration * 100 / unique(.data$n_subplots),
      approx_nr_regeneration_ha =
        sum(.data$approx_nr_regeneration) / unique(.data$plotarea_ha),
      approx_rubbing_damage_perc = pmin(
        sum(.data$rubbing_damage_number, na.rm = TRUE) * 100 /
        sum(.data$nr_tmp, na.rm = TRUE), 100),
      rubbing_damage_number = sum(.data$rubbing_damage_number, na.rm = TRUE),
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
      rubbing_damage_number_ha =
        .data$rubbing_damage_number / .data$plotarea_ha,
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
      -"not_na_rubbing", -"rubbing_damage_number"
    )

  attr(by_plot_species, "database") <- attr(data_regeneration, "database")
  attr(by_plot_species, "forrescalc") <- attr(data_regeneration, "forrescalc")

  return(by_plot_species)
}
