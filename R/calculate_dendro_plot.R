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
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_dendro_plot(data_dendro, data_deadwood)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by inner_join n_distinct mutate summarise ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot <- function(data_dendro, data_deadwood) {
  by_plot <- data_dendro %>%
    mutate(
      species_alive = ifelse(.data$alive_dead == 11, .data$species, NA)
    ) %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$plottype
    ) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species_alive, na.rm = TRUE),
      number_of_trees_ha =
        sum((.data$alive_dead == 11) * .data$individual / .data$plotarea_ha),
      stem_number_ha =
          sum((.data$alive_dead == 11) * .data$tree_number / .data$plotarea_ha),
      basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha * .data$tree_number),
      basal_area_dead_m2_ha = sum(.data$basal_area_dead_m2_ha * .data$tree_number),
      vol_alive_m3_ha = sum(.data$vol_alive_m3_ha * .data$tree_number),
      vol_dead_standing_m3_ha = sum(.data$vol_dead_standing_m3_ha * .data$tree_number),
      vol_stem_alive_m3_ha = sum(.data$vol_stem_alive_m3_ha * .data$tree_number),
      vol_stem_dead_standing_m3_ha = sum(.data$vol_stem_dead_standing_m3_ha * .data$tree_number)
    ) %>%
    ungroup() %>%
    left_join(
      data_deadwood %>%
        group_by(.data$plot_id, .data$year, .data$period) %>%
        summarise(
          volume_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period")
    ) %>%
    mutate(
      volume_log_m3_ha =
        ifelse(
          is.na(.data$volume_log_m3_ha) & .data$plottype %in% c(20, 30) &
            !is.na(.data$vol_alive_m3_ha),
          0, .data$volume_log_m3_ha
        ),
      plottype = NULL,
      volume_deadwood_m3_ha = .data$vol_dead_standing_m3_ha + .data$volume_log_m3_ha,
      stems_per_tree = .data$stem_number_ha / .data$number_of_trees_ha
    )

  return(by_plot)
}
