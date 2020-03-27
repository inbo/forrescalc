#' aggregate parameters by plot and year
#'
#' This function calculates for each plot and year some values per hectare: number of tree species, number of trees, basal area, and volume.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns plot, year, number_of_tree_species, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_dendro_plot_year(data_dendro, data_deadwood)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by inner_join n n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot_year <- function(data_dendro, data_deadwood) {
  by_plot_year <- data_dendro %>%
    mutate(
      species_alive = ifelse(.data$AliveDead == 11, .data$species, NA)
    ) %>%
    group_by(.data$plot_id, .data$year, .data$period, .data$Plottype) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species_alive, na.rm = TRUE),
      number_of_trees_ha =
        round(sum(.data$AliveDead == 11) / unique(.data$subcirclearea_ha)),
      basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
      basal_area_snag_m2_ha = sum(.data$basal_area_snag_m2_ha),
      volume_alive_m3_ha = sum(.data$volume_alive_m3_ha),
      volume_snag_m3_ha = sum(.data$volume_snag_m3_ha)
    ) %>%
    ungroup() %>%
    left_join(
      data_deadwood %>%
        group_by(.data$plot_id, .data$year, .data$period, .data$Plottype) %>%
        summarise(
          volume_log_m3_ha = sum(.data$CalcVolume_m3) / ((pi * .data$rA4 ^ 2)/10000)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period")
    ) %>%
    mutate(
      volume_deadwood_m3_ha = .data$volume_snag_m3_ha + .data$volume_log_m3_ha
    )

  return(by_plot_year)
}
