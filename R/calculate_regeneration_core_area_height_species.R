#' aggregate regeneration parameters by plot, height, species and year
#'
#' This function calculates for each plot, height, species and year the number
#' of regeneration per ha (or interval with mean and confidence interval
#' using a log transformation), and the number and percentage of
#' subplots in which the species is regenerating.
#' This calculation is designed for core areas, that consist of different
#' subplots.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, species, year, height,
#' nr_of_subplots_with_regeneration, perc_subplots_with_regeneration,
#' rubbing_damage_perc, mean_number_of_regeneration_ha,
#' lci_number_of_regeneration_ha, uci_number_of_regeneration_ha and
#' approx_nr_regeneration_ha.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_regeneration_CA <-
#'   load_data_regeneration(
#'     "C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb",
#'     plottype = "CA"
#'   )
#' calculate_regeneration_core_area_height_species(data_regeneration_CA)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_core_area_height_species <- function(data_regeneration) {
  by_plot_species <- data_regeneration %>%
    group_by(.data$plot_id, .data$period) %>%
    mutate(
      n_subplots = n_distinct(.data$subplot_id)
    ) %>%
    ungroup() %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$height_class,
      .data$species, .data$plotarea_ha
    ) %>%
    summarise(
      nr_of_subplots_with_regeneration = n_distinct(.data$subplot_id),
      perc_subplots_with_regeneration =
        .data$nr_of_subplots_with_regeneration * 100 / unique(.data$n_subplots),
      rubbing_damage_perc =
        sum(.data$rubbing_damage_number, na.rm = TRUE) * 100 /
        sum(.data$nr_of_regeneration * (.data$subcircle == "A2"), na.rm = TRUE),
      not_na_rubbing = sum(!is.na(.data$rubbing_damage_number)),
      interval =
        sum_intervals(
          var_min = .data$min_number_of_regeneration,
          var_max = .data$max_number_of_regeneration,
          transformation = "log", na_rm = TRUE
        ),
      approx_nr_regeneration_ha =
        sum(.data$approx_nr_regeneration) / unique(.data$plotarea_ha)
    ) %>%
    ungroup() %>%
    mutate(
      mean_number_of_regeneration_ha = .data$interval$sum / .data$plotarea_ha,
      lci_number_of_regeneration_ha = .data$interval$lci / .data$plotarea_ha,
      uci_number_of_regeneration_ha = .data$interval$uci / .data$plotarea_ha,
      rubbing_damage_perc =
        ifelse(
          .data$not_na_rubbing > 0 & .data$rubbing_damage_perc > 0,
          .data$rubbing_damage_perc,
          NA
        )
    ) %>%
    select(
      -.data$interval, -.data$plotarea_ha,
      -.data$not_na_rubbing
    )

  return(by_plot_species)
}
