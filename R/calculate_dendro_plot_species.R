#' aggregate parameters by plot and tree species
#'
#' This function calculates for each plot, tree species and year some values per hectare: number of trees, basal area, and volume.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns plot, year, tree_species, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_dendro_plot_species(data_dendro, data_deadwood)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by left_join mutate summarise ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot_species <- function(data_dendro, data_deadwood) {
  by_plot_species <- data_dendro %>%
    group_by(
      .data$plot_id, .data$forest_reserve, .data$year, .data$period,
      .data$species, .data$plottype
    ) %>%
    summarise(
      number_of_trees_ha =
        round(
          sum((.data$alive_dead == 11) * .data$individual / .data$plotarea_ha)
        ),
      stem_number_ha =
        round(
          sum((.data$alive_dead == 11) * .data$tree_number / .data$plotarea_ha)
        ),
      basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha * .data$tree_number),
      basal_area_snag_m2_ha = sum(.data$basal_area_snag_m2_ha * .data$tree_number),
      volume_alive_m3_ha = sum(.data$volume_alive_m3_ha * .data$tree_number),
      volume_snag_m3_ha = sum(.data$volume_snag_m3_ha * .data$tree_number),
      volume_stem_alive_m3_ha = sum(.data$volume_stem_alive_m3_ha * .data$tree_number),
      volume_stem_snag_m3_ha = sum(.data$volume_stem_snag_m3_ha * .data$tree_number)
    ) %>%
    ungroup() %>%
    left_join(
      data_deadwood %>%
        group_by(.data$plot_id, .data$year, .data$period, .data$species, .data$plottype) %>%
        summarise(
          volume_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period", "species", "plottype")
    ) %>%
    mutate(
      volume_log_m3_ha =
        ifelse(
          is.na(.data$volume_log_m3_ha) & .data$plottype %in% c(20, 30) &
            !is.na(.data$volume_alive_m3_ha),
          0, .data$volume_log_m3_ha
        ),
      volume_deadwood_m3_ha = .data$volume_snag_m3_ha + .data$volume_log_m3_ha,
      stems_per_tree = .data$stem_number_ha / .data$number_of_trees_ha
    )

  return(by_plot_species)
}
