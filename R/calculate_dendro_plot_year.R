#' aggregate parameters by plot and year
#'
#' This function calculates for each plot and year some values per hectare: number of tree species, number of trees, basal area, and volume.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns plot, year, number_of_tree_species, number_of_trees, basal_area, volume
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot_year <- function(data_dendro) {
  by_plot_year <- data_dendro %>%
    group_by(.data$plot_id, .data$year, .data$series) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species),
      number_of_trees = n(),
      basal_area = sum(.data$BasalArea_m2) / unique(.data$Area_ha),
      volume_stem = sum(.data$Vol_stem_m3) / unique(.data$Area_ha)
    ) %>%
    ungroup()

  return(by_plot_year)
}
