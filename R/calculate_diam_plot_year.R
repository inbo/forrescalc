#' aggregate parameters by diameter class, plot and year
#'
#' This function calculates for each plot, year and diamter class some values per hectare: number of stems and basal area (for coppice trees based on data on shoot level), basal area and volume on tree level (for coppice trees based on data on tree level), number and volume of logs.
#'
#' @inheritParams calculate_dendrometry
#' @inheritParams calculate_diam_plot_species
#'
#' @return dataframe with columns plot, year, DBHClass_5cm, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_shoots <-
#'   load_data_shoots("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_diam_plot_year(data_stems, data_dendro, data_deadwood)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% filter group_by n summarise ungroup
#' @importFrom rlang .data
#'
calculate_diam_plot_year <- function(data_stems, data_dendro, data_deadwood) {
  by_diam_plot_year <- data_stems %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$DBHClass_5cm,
      .data$AliveDead
    ) %>%
    summarise(
      stem_number_ha = round(sum(n() / .data$plotarea_ha)),
      basal_area_m2_ha = sum(.data$basal_area_m2 / .data$plotarea_ha)
    ) %>%
    ungroup() %>%
    pivot_wider(
      names_from = "AliveDead",
      values_from = c("stem_number_ha", "basal_area_m2_ha")
    ) %>%
    rename(
      stem_number_alive_ha = .data$stem_number_ha_11,
      stem_number_snag_ha = .data$stem_number_ha_12,
      basal_area_shoot_alive_m2_ha = .data$basal_area_m2_ha_11,
      basal_area_shoot_snag_m2_ha = .data$basal_area_m2_ha_12
    ) %>%
    left_join(
      data_dendro %>%
        group_by(
          .data$plot_id, .data$year, .data$period, .data$DBHClass_5cm
        ) %>%
        summarise(
          basal_area_tree_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
          basal_area_tree_snag_m2_ha = sum(.data$basal_area_snag_m2_ha),
          volume_tree_alive_m3_ha = sum(.data$volume_alive_m3_ha),
          volume_tree_snag_m3_ha = sum(.data$volume_snag_m3_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period", "DBHClass_5cm")
    ) %>%
    left_join(
      data_deadwood %>%
        group_by(
          .data$plot_id, .data$year, .data$period, .data$DBHClass_5cm
        ) %>%
        summarise(
          log_number_ha = round(sum(n() / .data$plotarea_ha)),
          volume_log_m3_ha = sum(.data$CalcVolume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period", "DBHClass_5cm")
    )
  return(by_diam_plot_year)
}
