#' aggregate parameters by diameter class, plot and year
#'
#' This function calculates for each plot, year and diameter class some values per hectare: number of stems, basal area and volume of standing trees (for coppice based on data on shoot level), and volume of logs (= lying deadwood).
#'
#' @inheritParams calculate_dendrometry
#' @inheritParams calc_variables_tree_level
#'
#' @return dataframe with columns plot, year, dbh_class_5cm, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
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
#' calculate_diam_plot(data_stems_calc, data_deadwood)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by left_join n rename summarise ungroup
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom rlang .data
#'
calculate_diam_plot <- function(data_stems_calc, data_deadwood) {
  by_diam_plot <- data_stems_calc %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$dbh_class_5cm
    ) %>%
    summarise(
      stem_number_alive_ha =
        sum(.data$stem_number_alive_ha),
      stem_number_dead_ha =
        sum(.data$stem_number_dead_ha),
      basal_area_alive_m2_ha =
        sum(.data$basal_area_alive_m2_ha),
      basal_area_dead_m2_ha =
        sum(.data$basal_area_dead_m2_ha),
      vol_alive_m3_ha =
        sum(.data$vol_alive_m3_ha),
      vol_dead_standing_m3_ha =
        sum(.data$vol_dead_standing_m3_ha),
      vol_bole_alive_m3_ha =
        sum(.data$vol_bole_alive_m3_ha),
      vol_bole_dead_m3_ha =
        sum(.data$vol_bole_dead_m3_ha)
    ) %>%
    ungroup() %>%
    full_join(
      data_deadwood %>%
        group_by(
          .data$plot_id, .data$year, .data$period, .data$dbh_class_5cm
        ) %>%
        summarise(
          vol_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period", "dbh_class_5cm")
    ) %>%
    replace_na(
      list(
        stem_number_alive_ha = 0,
        stem_number_dead_ha = 0,
        basal_area_alive_m2_ha = 0,
        basal_area_dead_m2_ha = 0,
        vol_alive_m3_ha = 0,
        vol_dead_m3_ha = 0,
        vol_bole_alive_m3_ha = 0,
        vol_bole_dead_m3_ha = 0,
        log_number_ha = 0,
        vol_log_m3_ha = 0
      )
    )

  return(by_diam_plot)
}
