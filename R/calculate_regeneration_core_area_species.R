#' aggregate regeneration parameters by plot, species and year
#'
#' This function calculates for each plot, species and year the number of
#' seedlings and established regeneration per ha (or interval with mean
#' and confidence interval), and the number and percentage of subplots in which
#' the species is regenerating.
#' This calculation is designed for core areas,that consist of different
#' subplots.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, species, year,
#' number_of_subplots_with_regeneration, perc_subplots_with_regeneration,
#' nr_established_ha, nr_seedlings_ha, mean_number_established_ha,
#' lci_number_established_ha, uci_number_established_ha,
#' mean_number_seedlings_ha, lci_number_seedlings_ha, uci_number_seedlings_ha.
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
#' calculate_regeneration_core_area_species(data_regeneration_CA)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_core_area_species <- function(data_regeneration) {
  by_plot_species <- data_regeneration %>%
    mutate(
      n_subplots = n_distinct(.data$subplot_id),
      nr_established_ha =
        ifelse(.data$subcircle == "A2",
               .data$nr_of_regeneration / .data$plotarea_ha, NA),
      min_number_established_ha =
        ifelse(.data$subcircle == "A2",
               .data$min_number_of_regeneration / .data$plotarea_ha, NA),
      max_number_established_ha =
        ifelse(.data$subcircle == "A2",
               .data$max_number_of_regeneration / .data$plotarea_ha, NA),
      nr_seedlings_ha =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$nr_of_regeneration / .data$plotarea_ha, NA),
      min_number_seedlings_ha =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$min_number_of_regeneration / .data$plotarea_ha, NA),
      max_number_seedlings_ha =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$max_number_of_regeneration / .data$plotarea_ha, NA)
    ) %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$species
    ) %>%
    summarise(
      number_of_subplots_with_regeneration = n_distinct(.data$subplot_id),
      perc_subplots_with_regeneration =
        .data$number_of_subplots_with_regeneration * 100 / unique(.data$n_subplots),
      nr_established_ha = sum(.data$nr_established_ha, na.rm = TRUE),
      not_na_established = sum(!is.na(.data$nr_established_ha)),
      established_interval =
        sum_intervals(
          var_min = .data$min_number_established_ha,
          var_max = .data$max_number_established_ha,
          transformation = "log", na_rm = TRUE
        ),
      nr_seedlings_ha = sum(.data$nr_seedlings_ha, na.rm = TRUE),
      not_na_seedlings = sum(!is.na(.data$nr_seedlings_ha)),
      seedlings_interval =
        sum_intervals(
          var_min = .data$min_number_seedlings_ha,
          var_max = .data$max_number_seedlings_ha,
          transformation = "log", na_rm = TRUE
        ),
      rubbing_damage_perc =
        sum(.data$rubbing_damage_number, na.rm = TRUE) * 100 /
        sum(.data$nr_of_regeneration * (.data$subcircle == "A2"), na.rm = TRUE),
      not_na_rubbing = sum(!is.na(.data$rubbing_damage_perc))
    ) %>%
    ungroup() %>%
    mutate(
      nr_established_ha =
        ifelse(
          .data$not_na_established > 0 & .data$nr_established_ha > 0,
          .data$nr_established_ha,
          NA
        ),
      mean_number_established_ha = .data$established_interval$sum,
      lci_number_established_ha = .data$established_interval$lci,
      uci_number_established_ha = .data$established_interval$uci,
      nr_seedlings_ha =
        ifelse(
          .data$not_na_seedlings > 0 & .data$nr_seedlings_ha > 0,
          .data$nr_seedlings_ha,
          NA
        ),
      mean_number_seedlings_ha = .data$seedlings_interval$sum,
      lci_number_seedlings_ha = .data$seedlings_interval$lci,
      uci_number_seedlings_ha = .data$seedlings_interval$uci,
      rubbing_damage_perc =
        ifelse(
          .data$not_na_rubbing > 0 & .data$rubbing_damage_perc > 0,
          .data$rubbing_damage_perc,
          NA
        )
    ) %>%
    select(
      -.data$established_interval, -.data$not_na_established,
      -.data$seedlings_interval, -.data$not_na_seedlings, -.data$not_na_rubbing
    )

  return(by_plot_species)
}
