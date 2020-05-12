#' aggregates parameters on regeneration of trees
#'
#' This function makes aggregations of tree generation data on the levels of
#' \itemize{
#'  \item plot, height class and year
#'  \item plot and year
#'  \item plot, height class, tree species and year
#' }
#' For core area plots it makes additional aggregations on the levels of
#' \itemize{
#'  \item core area, tree species and year
#'  \item core area, height class, tree species and year
#' }
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_regeneration(data_regeneration)
#' }
#'
#' @param data_regeneration dataframe on tree regeneration with variables ...
#'
#' @return List of dataframes that are mentioned in the above description
#'
#' @importFrom dplyr %>% filter
#' @importFrom rlang .data
#'
#' @export
#'
calculate_regeneration <- function(data_regeneration) {
  by_plot_height <- calculate_regeneration_plot_height(data_regeneration)
  by_plot <- calculate_regeneration_plot(data_regeneration)
  by_plot_height_species <-
    calculate_regeneration_plot_height_species(data_regeneration)
  data_regeneration_CA <- data_regeneration %>%
    filter(.data$plottype == 30)
  by_ca_species <- calculate_regeneration_core_area_species(data_regeneration_CA)
  by_ca_height_species <-
    calculate_regeneration_core_area_height_species(data_regeneration_CA)

  return(
    list(
      regeneration_by_plot_height = by_plot_height,
      regeneration_by_plot = by_plot,
      regeneration_by_plot_height_species = by_plot_height_species,
      regeneration_by_core_area_species = by_ca_species,
      regeneration_by_core_area_height_species = by_ca_height_species
    )
  )
}
