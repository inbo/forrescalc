#' aggregate parameters by plot and year
#'
#' This function calculates for each plot and year some values per hectare: number of tree species, number of trees, basal area and volume.
#'
#' @inheritParams calculate_dendrometry
#' @param data_dendro_calc dataframe on tree measures with variables plot_id, plottype, tree_measure_id, date_dendro, dbh_mm, height_m, species, alive_dead, decaystage, vol_tot_m3, basal_area_m2, period, OldID, year, subcircle, plotarea_ha,... (output of function calc_variables_tree_level())
#'
#' @return dataframe with columns plot, year, number_of_tree_species, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_shoots <-
#'   load_data_shoots("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' height_model <- load_height_models("C:/bosreservaten/Hoogtemodellen/")
#' data_stems_calc <- calc_variables_stem_level(data_stems, height_model)
#' data_dendro_calc <- calc_variables_tree_level(data_dendro, data_stems_calc)
#' calculate_dendro_plot(data_dendro_calc, data_deadwood)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by inner_join n_distinct mutate summarise ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot <- function(data_dendro_calc, data_deadwood) {
  by_plot <- data_dendro_calc %>%
    mutate(
      species_alive = ifelse(.data$alive_dead == 11, .data$species, NA)
    ) %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$plottype
    ) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species_alive, na.rm = TRUE),
      number_of_trees_ha = sum(.data$number_of_trees_alive_ha),
      stem_number_ha =
          sum((.data$alive_dead == 11) * .data$nr_of_stems / .data$plotarea_ha),
      basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
      basal_area_dead_m2_ha = sum(.data$basal_area_dead_m2_ha),
      vol_alive_m3_ha = sum(.data$vol_alive_m3_ha),
      vol_dead_standing_m3_ha = sum(.data$vol_dead_standing_m3_ha),
      vol_bole_alive_m3_ha = sum(.data$vol_bole_alive_m3_ha),
      vol_bole_dead_m3_ha = sum(.data$vol_bole_dead_m3_ha)
    ) %>%
    ungroup() %>%
    left_join(
      data_deadwood %>%
        group_by(.data$plot_id, .data$year, .data$period) %>%
        summarise(
          vol_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period")
    ) %>%
    mutate(
      vol_log_m3_ha =
        ifelse(
          is.na(.data$vol_log_m3_ha) & .data$plottype %in% c("CP", "CA") &
            !is.na(.data$vol_alive_m3_ha),
          0, .data$vol_log_m3_ha
        ),
      plottype = NULL,
      vol_deadw_m3_ha = .data$vol_dead_standing_m3_ha + .data$vol_log_m3_ha,
      stems_per_tree = .data$stem_number_ha / .data$number_of_trees_ha
    )

  return(by_plot)
}
