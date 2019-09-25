#' aggregate parameters by plot, tree species and year
#'
#' This function calculates for each plot, tree species and year some values per hectare: number of trees, basal area, and volume.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns plot, year, tree_species, number_of_trees, basal_area, volume
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n summarise ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot_species_year <- function(data_dendro) {
  by_plot_species_year <- data_dendro %>%
    group_by(.data$plot_id, .data$year, .data$series, .data$species) %>%
    summarise(
      number_of_trees = n(),
      basal_area = sum(.data$BasalArea_m2) / unique(.data$Area_ha),
      volume_stem = sum(.data$Vol_stem_m3) / unique(.data$Area_ha)
    ) %>%
    ungroup()

  return(by_plot_species_year)
}
