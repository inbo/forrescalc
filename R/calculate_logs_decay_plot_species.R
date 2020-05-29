#' aggregate parameters by decay stage, plot, tree species and year
#'
#' This function calculates for each plot, tree species and year the volume logs per hectare and per decay stage.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns plot, year, tree_species, decaystage, volume_log_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_logs_decay_plot_species(data_deadwood)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by summarise ungroup
#' @importFrom rlang .data
#'
calculate_logs_decay_plot_species <- function(data_deadwood) {
  by_decay_plot_species <-
    data_deadwood %>%
      group_by(
        .data$plot_id, .data$year, .data$period, .data$species, .data$decaystage
      ) %>%
      summarise(
        volume_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
      ) %>%
      ungroup()

  return(by_decay_plot_species)
}
