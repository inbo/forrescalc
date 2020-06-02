#' calculate species number by plot, tree height class and year
#'
#' This function calculates for each plot, tree height class and year the number of species, total number of trees and rubbing damage percentage for regeneration.  For core area plots, these variables are calculated for each subplot.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, year, height_class and number_of_tree_species
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_regeneration_plot_height(data_regeneration)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_height <- function(data_regeneration) {
  by_plot_height <- data_regeneration %>%
    mutate(
      plotarea_ha = ifelse(.data$plottype == 30, 0.01, .data$plotarea_ha)
    ) %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$height_class,
      .data$subplot_id
    ) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species, na.rm = TRUE),
      min_number_of_trees_ha = sum(.data$min_number_of_trees / .data$plotarea_ha),
      max_number_of_trees_ha = sum(.data$max_number_of_trees / .data$plotarea_ha),
      rubbing_damage_perc = sum(.data$rubbing_damage_number) * 100 / sum(.data$reg_number)
    ) %>%
    ungroup()

  return(by_plot_height)
}
