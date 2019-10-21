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
#' @importFrom dplyr %>% group_by inner_join mutate n summarise ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot_species_year <- function(data_dendro, data_deadwood) {
  by_plot_species_year <- data_dendro %>%
    group_by(.data$plot_id, .data$year, .data$period, .data$species) %>%
    summarise(
      number_of_trees_ha = n() / unique(.data$Area_ha),
      basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
      basal_area_dead_m2_ha = sum(.data$basal_area_dead_m2_ha),
      volume_alive_m3_ha = sum(.data$volume_alive_m3_ha),
      volume_snag_m3_ha = sum(.data$volume_snag_m3_ha)
    ) %>%
    ungroup() %>%
    inner_join(
      data_deadwood %>%
        group_by(.data$plot_id, .data$year, .data$period, .data$species) %>%
        summarise(
          volume_log_m3_ha = sum(.data$CalcVolume_m3) / ((pi * 18 ^ 2)/10000)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period", "species")
    ) %>%
    mutate(
      volume_deadwood_m3_ha = .data$volume_snag_m3_ha + .data$volume_log_m3_ha
    )

  return(by_plot_species_year)
}
