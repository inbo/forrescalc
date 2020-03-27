#' aggregate parameters by diameter class, plot, tree species and year
#'
#' This function calculates for each plot, tree species, year and diameter class some values per hectare: number of trees, basal area, and volume.
#'
#' @param data_stems data on stems (shoots and trees) as given from the function compose_stem_data()
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns plot, year, tree_species, DBHClass_5cm, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_shoots <-
#'   load_data_shoots("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' calculate_diam_plot_species_year(data_dendro, data_stems)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% filter group_by n summarise ungroup
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#'
calculate_diam_plot_species_year <- function(data_dendro, data_stems) {
  by_diam_plot_species_year <- data_stems %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$species,
      .data$DBHClass_5cm, .data$AliveDead
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
      basal_area_alive_m2_ha = .data$basal_area_m2_ha_11,
      basal_area_snag_m2_ha = .data$basal_area_m2_ha_12
    )

  return(by_diam_plot_species_year)
}
