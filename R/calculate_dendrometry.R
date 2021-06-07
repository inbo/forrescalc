#' aggregates parameters on dendrometry of trees
#'
#' This function makes aggregations of individual tree measures on the levels of
#' \itemize{
#'  \item plot and year
#'  \item plot, tree species and year
#'  \item diameter class, plot and year
#'  \item diameter class, plot, tree species and year
#' }
#' and it makes aggregations of volume data on logs on the levels of
#' \itemize{
#'  \item decay stage, plot and year
#'  \item decay stage, plot, tree species and year
#' }
#'
#' @param data_dendro_calc dataframe on tree measures with variables plot_id, plottype, tree_measure_id, date_dendro, dbh_mm, height_m, species, alive_dead, decaystage, vol_tot_m3, basal_area_m2, period, OldID, year, subcircle, plotarea_ha,... (output of function calc_variables_tree_level())
#' @param data_deadwood dataframe on logs with variables plot_id, plottype, date_dendro, species, decaystage, calc_volume_m3, period and year (output of function load_data_deadwood())
#' @param data_stems_calc dataframe on stems (shoots and trees) as given from the function calc_variables_stem_level()
#'
#' @return List of dataframes that are mentioned in the above description
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_shoots <-
#'   load_data_shoots("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' data_dendro_calc <- calc_variables_tree_level(data_dendro, data_stems, height_model)
#' data_stems_calc <- calc_variables_stem_level(data_stems, heightmodel)
#' calculate_dendrometry(data_dendro_calc, data_deadwood, data_stems_calc)
#' }
#'
#' @export
#'
calculate_dendrometry <- function(data_dendro_calc, data_deadwood, data_stems_calc) {
  by_plot <- calculate_dendro_plot(data_dendro_calc, data_deadwood)
  by_plot_species <-
    calculate_dendro_plot_species(data_dendro_calc, data_deadwood)
  by_decay_plot <- calculate_logs_decay_plot(data_deadwood)
  by_decay_plot_species <-
    calculate_logs_decay_plot_species(data_deadwood)
  by_diam_plot <- calculate_diam_plot(data_stems_calc, data_deadwood)
  by_diam_plot_species <-
    calculate_diam_plot_species(data_stems_calc, data_deadwood)

  return(
    list(
      dendro_by_plot = by_plot,
      dendro_by_plot_species = by_plot_species,
      dendro_by_diam_plot = by_diam_plot,
      dendro_by_diam_plot_species = by_diam_plot_species,
      logs_by_decay_plot = by_decay_plot,
      logs_by_decay_plot_species = by_decay_plot_species
    )
  )
}
