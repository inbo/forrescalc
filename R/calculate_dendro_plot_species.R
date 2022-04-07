#' aggregate parameters by plot and tree species
#'
#' This function calculates for each plot, tree species and year some values
#' per hectare: number of trees, basal area and volume.
#'
#' @inheritParams calculate_dendro_plot
#'
#' @return dataframe with columns plot, year, tree_species, number_of_trees_ha,
#' basal_area_m2_ha, volume_m3_ha
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
#' data_stems_calc <- calc_variables_stem_level(data_stems, heightmodel)
#' data_dendro_calc <- calc_variables_tree_level(data_dendro, data_stems_calc)
#' calculate_dendro_plot_species(data_dendro_calc, data_deadwood)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by left_join mutate summarise ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot_species <- function(data_dendro_calc, data_deadwood) {
  by_plot_species <- data_dendro_calc %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$species, .data$plottype
    ) %>%
    summarise(
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
        group_by(.data$plot_id, .data$year, .data$period, .data$species) %>%
        summarise(
          vol_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period", "species")
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

  return(by_plot_species)
}
