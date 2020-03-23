#' aggregate parameters by diameter class, plot and year
#'
#' This function calculates for each plot, year and diamter class some values per hectare: number of trees, basal area, and volume.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns plot, year, DBHClass_5cm, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_diam_plot_year(data_dendro)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% filter group_by n summarise ungroup
#' @importFrom rlang .data
#'
calculate_diam_plot_year <- function(data_dendro) {
  by_diam_plot_year <- data_dendro %>%
    filter(.data$AliveDead == 11) %>%
    group_by(.data$plot_id, .data$year, .data$period, .data$DBHClass_5cm) %>%
    summarise(
      basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
      volume_alive_m3_ha = sum(.data$volume_alive_m3_ha)
    ) %>%
    ungroup()
  return(by_diam_plot_year)
}
