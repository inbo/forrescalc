#' aggregate parameters by plot, tree height class, species and year
#'
#' This function calculates for each plot, tree height class, species and year the number of trees and rubbing damage percentage per hectare for regeneration.  For core area plots, these variables are calculated for each subplot.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, year, height_class, species and number_of_trees_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_regeneration_plot_height_species(data_regeneration)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_height_species <- function(data_regeneration) {
  by_plot_height_species <- data_regeneration %>%
    mutate(
      plotarea_ha = ifelse(.data$plottype == 30, 0.01, .data$plotarea_ha)
    ) %>%
    group_by(
      .data$plot_id, .data$forest_reserve, .data$year, .data$period,
      .data$height_class, .data$species, .data$subplot_id
    ) %>%
    summarise(
      min_number_of_trees_ha = sum(.data$min_number_of_trees / .data$plotarea_ha),
      max_number_of_trees_ha = sum(.data$max_number_of_trees / .data$plotarea_ha),
      rubbing_damage_perc = sum(.data$rubbing_damage_number) * 100 / sum(.data$reg_number)
    ) %>%
    ungroup()

  return(by_plot_height_species)
}
