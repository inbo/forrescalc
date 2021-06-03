#' aggregate parameters by diameter class, plot, tree species and year
#'
#' This function calculates for each plot, tree species, year and diameter class some values per hectare: number of stems, basal area and volume of standing trees (for coppice based on data on shoot level), number and volume of logs (= lying deadwood).
#'
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns plot, year, tree_species, dbh_class_5cm, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_shoots <-
#'   load_data_shoots("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' height_model <- load_height_models("C:/bosreservaten/Hoogtemodellen/")
#' data_stems_calc <- calc_variables_stem_level(data_stems, heightmodel)
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_diam_plot_species(data_stems_calc, data_deadwood)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n summarise ungroup
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider replace_na
#'
calculate_diam_plot_species <-
  function(data_stems_calc, data_deadwood) {
  by_diam_plot_species <- data_stems_calc %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$species,
      .data$dbh_class_5cm, .data$alive_dead
    ) %>%
    summarise(
      stem_number_ha = sum(1 / .data$plotarea_ha),
      basal_area_m2_ha = sum(.data$basal_area_m2 / .data$plotarea_ha)
    ) %>%
    ungroup() %>%
    pivot_wider(
      names_from = "alive_dead",
      values_from = c("stem_number_ha", "basal_area_m2_ha")
    ) %>%
    rename(
      stem_number_alive_ha = .data$stem_number_ha_11,
      stem_number_dead_ha = .data$stem_number_ha_12,
      basal_area_shoot_alive_m2_ha = .data$basal_area_m2_ha_11,
      basal_area_shoot_dead_m2_ha = .data$basal_area_m2_ha_12
    ) %>%
    left_join(
      data_dendro %>%
        group_by(
          .data$plot_id, .data$year, .data$period, .data$species,
          .data$dbh_class_5cm
        ) %>%
        summarise(
          basal_area_tree_alive_m2_ha =
            sum(.data$basal_area_alive_m2_ha * .data$tree_number),
          basal_area_tree_dead_m2_ha =
            sum(.data$basal_area_dead_m2_ha * .data$tree_number),
          vol_tree_alive_m3_ha =
            sum(.data$vol_alive_m3_ha * .data$tree_number),
          vol_tree_dead_m3_ha =
            sum(.data$vol_dead_standing_m3_ha * .data$tree_number)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period", "species", "dbh_class_5cm")
    ) %>%
    full_join(
      data_deadwood %>%
        group_by(
          .data$plot_id, .data$year, .data$period, .data$species,
          .data$dbh_class_5cm
        ) %>%
        summarise(
          log_number_ha = sum(n() / .data$plotarea_ha),
          vol_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period", "species", "dbh_class_5cm")
    ) %>%
    replace_na(
      list(
        stem_number_alive_ha = 0,
        stem_number_dead_ha = 0,
        basal_area_shoot_alive_m2_ha = 0,
        basal_area_shoot_dead_m2_ha = 0,
        basal_area_tree_alive_m2_ha = 0,
        basal_area_tree_dead_m2_ha = 0,
        vol_tree_alive_m3_ha = 0,
        vol_tree_dead_m3_ha = 0,
        log_number_ha = 0,
        vol_log_m3_ha = 0
      )
    )

  return(by_diam_plot_species)
}
