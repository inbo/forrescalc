#' aggregate parameters by plot
#'
#' This function compares for each plot the differences between years for: number of tree species, number of trees, basal area, and volume (calculated per hectare). It gives results for differences between subsequent measures (based on 'period') and between the last and the first measure.
#'
#' @param by_plot_species_year dataframe with values for each plot, species and year, which is the result of the calculation by function calculate_dendro_plot_species_year()
#'
#' @return dataframe with columns plot, number_of_tree_species, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' by_plot_species_year <-
#'   calculate_dendro_plot_species_year(data_dendro, data_deadwood)
#' calculate_dendro_plot_species(by_plot_species_year)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows filter group_by inner_join mutate summarise transmute ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot_species <- function(by_plot_species_year) {
  #data from long to wide
  by_plot_species <- by_plot_species_year %>%
    pivot_wider(
      names_from = "period",
      values_from =
        c(.data$year, .data$number_of_trees_ha,
          .data$basal_area_alive_m2_ha, .data$basal_area_snag_m2_ha,
          .data$volume_alive_m3_ha, .data$volume_snag_m3_ha,
          .data$volume_log_m3_ha, .data$volume_deadwood_m3_ha)
    ) %>%
    transmute(  #calculate: make the comparison
      .data$plot_id,
      .data$species,
      period_diff = "2 - 1",
      year_diff = paste(.data$year_2, .data$year_1, sep = " - "),
      number_of_trees_ha_diff =
        .data$number_of_trees_ha_2 - .data$number_of_trees_ha_1,
      basal_area_alive_m2_ha_diff =
        .data$basal_area_alive_m2_ha_2 - .data$basal_area_alive_m2_ha_1,
      basal_area_snag_m2_ha_diff =
        .data$basal_area_snag_m2_ha_2 - .data$basal_area_snag_m2_ha_1,
      volume_alive_m3_ha_diff =
        .data$volume_alive_m3_ha_2 - .data$volume_alive_m3_ha_1,
      volume_snag_m3_ha_diff =
        .data$volume_snag_m3_ha_2 - .data$volume_snag_m3_ha_1,
      volume_log_m3_ha_diff =
        .data$volume_log_m3_ha_2 - .data$volume_log_m3_ha_1,
      volume_deadwood_m3_ha_diff =
        .data$volume_deadwood_m3_ha_2 - .data$volume_deadwood_m3_ha_1
    )

  return(by_plot_species)
}
