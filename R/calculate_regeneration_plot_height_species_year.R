#' aggregate parameters by plot, tree height class, species and year
#'
#' This function calculates for each plot, tree height class, species and year the number of trees per hectare for regeneration.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, year, height_class, species and number_of_trees_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_regeneration_plot_height_species_year(data_regeneration)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_height_species_year <- function(data_regeneration) {
  by_plot_height_species_year <- data_regeneration %>%
    group_by(.data$plot_id, .data$year, .data$period, .data$height_class, .data$species) %>%
    summarise(
      min_number_of_trees_ha = sum(.data$min_number_of_trees) / unique(.data$area_ha),
      max_number_of_trees_ha = sum(.data$max_number_of_trees) / unique(.data$area_ha)
    ) %>%
    ungroup()

  return(by_plot_height_species_year)
}
