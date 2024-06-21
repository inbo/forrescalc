#' aggregates parameters on regeneration of trees
#'
#' This function makes aggregations of tree generation data on the levels of
#' \itemize{
#'  \item plot and year (and subplot for core area)
#'  \item plot, height class and year (and subplot for core area)
#'  \item plot, tree species and year (and subplot for core area)
#'  \item plot, height class, tree species and year (and subplot for core area)
#' }
#' For core area plots it makes additional aggregations on the levels of
#' \itemize{
#'  \item core area, tree species and year
#'  \item core area, height class, tree species and year
#' }
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_regeneration <- load_data_regeneration(path_to_fieldmapdb)
#' calculate_regeneration(data_regeneration)
#'
#' @param data_regeneration dataframe on tree regeneration with variables
#' `plot_id`, `plottype`, `subplot_id`, `height_class`, `species`,
#' `nr_of_regeneration`, `rubbing_damage_number`, `period`, `year`, `subcircle`,
#' `plotarea_ha`, `min_number_of_regeneration` and `max_number_of_regeneration`.
#'
#' @return List of dataframes that are mentioned in the above description
#'
#' @importFrom dplyr %>% filter
#' @importFrom rlang .data
#'
#' @export
#'
calculate_regeneration <- function(data_regeneration) {
  by_plot <- calc_regeneration_plot(data_regeneration)
  by_plot_height <- calc_regeneration_plot_height(data_regeneration)
  by_plot_species <- calc_regeneration_plot_species(data_regeneration)
  by_plot_height_species <-
    calc_regeneration_plot_height_species(data_regeneration)
  data_regeneration_ca <- data_regeneration %>%
    filter(.data$plottype == "CA")
  by_ca_species <-
    calc_regeneration_core_area_species(data_regeneration_ca)
  by_ca_height_species <-
    calc_regeneration_core_area_height_species(data_regeneration_ca)

  return(
    list(
      reg_by_plot = by_plot,
      reg_by_plot_height = by_plot_height,
      reg_by_plot_species = by_plot_species,
      reg_by_plot_height_species = by_plot_height_species,
      reg_by_core_area_species = by_ca_species,
      reg_by_core_area_height_species = by_ca_height_species
    )
  )
}
