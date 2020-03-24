#' aggregates parameters on dendrometry of trees
#'
#' This function makes aggregations of individual tree measures on the levels of
#' \itemize{
#'  \item plot and year
#'  \item plot, tree species and year
#'  \item individual trees: statuses in different years
#'  \item diameter class, plot and year
#'  \item diameter class, plot, tree species and year
#' }
#' and it makes aggregations of volume data on logs on the levels of
#' \itemize{
#'  \item decay stage, plot and year
#'  \item decay stage, plot, tree species and year
#' }
#'
#' @param data_dendro dataframe on tree measures with variables plot_id, tree_measure_id, date_dendro, DBH_mm, Height_m, species, AliveDead, decaystage, Adjust_Vol_tot_m3, AdjustBasalArea_m2, period, OldID, year, plottype, plotarea_ha,... (output of function load_data_dendrometry())
#' @param data_deadwood dataframe on logs with variables plot_id, date_dendro, species, decaystage, CalcVolume_m3, period and year (output of function load_data_deadwood())
#'
#' @return List of dataframes that are mentioned in the above description
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_dendrometry(data_dendro, data_deadwood)
#' }
#'
#' @export
#'
calculate_dendrometry <- function(data_dendro, data_deadwood) {
  by_plot_year <- calculate_dendro_plot_year(data_dendro, data_deadwood)
  by_plot_species_year <-
    calculate_dendro_plot_species_year(data_dendro, data_deadwood)
  by_decay_plot_year <-
    calculate_logs_decay_plot_year(data_deadwood)
  by_decay_plot_species_year <-
    calculate_logs_decay_plot_species_year(data_deadwood)
  status_tree <- summarise_status(data_dendro)
  by_diam_plot_year <- calculate_diam_plot_year(data_dendro)
  by_diam_plot_species_year <- calculate_diam_plot_species_year(data_dendro)

  return(
    list(
      dendro_by_plot_year = by_plot_year,
      dendro_by_plot_species_year = by_plot_species_year,
      dendro_status_tree = status_tree,
      dendro_by_diam_plot_year = by_diam_plot_year,
      dendro_by_diam_plot_species_year = by_diam_plot_species_year,
      logs_by_decay_plot_year = by_decay_plot_year,
      logs_by_decay_plot_species_year = by_decay_plot_species_year
    )
  )
}
