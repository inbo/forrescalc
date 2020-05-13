#' aggregate parameters by plot, height class and species
#'
#' This function compares for each plot, height class and species the differences between years for the number of individuals per hectare. It gives results for differences between subsequent measures (based on 'period') and between the last and the first measure.
#'
#' @param by_plot_height_species dataframe with values for each plot, height class, species and year, which is the result of the calculation by function calculate_regeneration_plot_height_species()
#'
#' @return dataframe with columns plot, height class, species and number_of_trees_ha_diff
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' by_plot_height_species <-
#'   calculate_regeneration_plot_height_species(data_regeneration)
#' calculate_regeneration_plot_height_species_diff(by_plot_height_species)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% mutate transmute
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'
calculate_regeneration_plot_height_species_diff <- function(by_plot_height_species) {
  #data from long to wide
  by_plot_height_species_diff <- by_plot_height_species %>%
    mutate(
      min_number_of_trees_ha =
        replace(
          .data$min_number_of_trees_ha,
          is.na(.data$min_number_of_trees_ha) & !is.na(.data$year),
          0
        ),
      max_number_of_trees_ha =
        replace(
          .data$max_number_of_trees_ha,
          is.na(.data$max_number_of_trees_ha) & !is.na(.data$year),
          0
        )
    ) %>%
    pivot_wider(
      names_from = "period",
      values_from =
        c(.data$year, .data$min_number_of_trees_ha, .data$max_number_of_trees_ha)
    ) %>%
    transmute(  #calculate: make the comparison
      .data$plot_id, .data$height_class, .data$species,
      period_diff = "2 - 1",
      year_diff = paste(.data$year_2, .data$year_1, sep = " - "),
      n_years = .data$year_2 - .data$year_1,
      min_number_of_trees_ha_diff =
        .data$min_number_of_trees_ha_2 - .data$min_number_of_trees_ha_1,
      max_number_of_trees_ha_diff =
        .data$max_number_of_trees_ha_2 - .data$max_number_of_trees_ha_1
    )

  return(by_plot_height_species_diff)
}
