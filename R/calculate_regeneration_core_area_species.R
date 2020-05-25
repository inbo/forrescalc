#' aggregate regeneration parameters by plot, species and year
#'
#' This function calculates for each plot, species and year the number of trees per ha, and the number and percentage of subplots in which the species is regenerating. This calculation is designed for core areas, that consist of different subplots.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, species, year, min_number_of_trees_ha, max_number_of_trees_ha, number_of_subplots_with_regeneration and perc_subplots_with_regeneration
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_regeneration_CA <-
#'   load_data_regeneration(
#'     "C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb",
#'     plottype = "Core area"
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
      n_subplots = n_distinct(.data$subplot_id)
    ) %>%
    group_by(
      .data$plot_id, .data$forest_reserve, .data$year, .data$period,
      .data$species
    ) %>%
    summarise(
      min_number_of_trees_ha = sum(.data$min_number_of_trees / .data$plotarea_ha),
      max_number_of_trees_ha = sum(.data$max_number_of_trees / .data$plotarea_ha),
      number_of_subplots_with_regeneration = n_distinct(.data$subplot_id),
      perc_subplots_with_regeneration =
        .data$number_of_subplots_with_regeneration * 100 / unique(.data$n_subplots),
      rubbing_damage_perc = sum(.data$rubbing_damage_number) / sum(.data$reg_number)
    ) %>%
    ungroup()

  return(by_plot_species)
}
