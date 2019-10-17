#' aggregate parameters by plot, tree species and year
#'
#' This function calculates for each plot, tree species and year some values per hectare: number of trees, basal area, and volume.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns plot, year, tree_species, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n summarise ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot_species_year <- function(data_dendro) {
  by_plot_species_year <- data_dendro %>%
    group_by(.data$plot_id, .data$year, .data$period, .data$species) %>%
    summarise(
      number_of_trees_ha = n() / unique(.data$Area_ha),
      basal_area_m2_ha = sum(.data$BasalArea_m2) / unique(.data$Area_ha),
      volume_stem_m3_ha = sum(.data$Vol_stem_m3) / unique(.data$Area_ha)
    ) %>%
    ungroup()

  return(by_plot_species_year)
}
