#' aggregate parameters by plot, tree height class, species and year
#'
#' This function calculates for each plot, tree height class, species and year the number of trees for regeneration.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, year, height_class, species and number_of_trees
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
      min_number_of_trees = sum(.data$min_number_of_trees),
      max_number_of_trees = sum(.data$max_number_of_trees)
    ) %>%
    ungroup()

  return(by_plot_height_species_year)
}
