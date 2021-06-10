#' aggregates parameters on vegetation of forests
#'
#' This function makes aggregations of vegetation data on the levels of
#' \itemize{
#'  \item plot and year
#'  \item subplot and year (only for plot type 'core area')
#'  \item plot, species and year (only for plot type 'core area')
#' }
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_vegetation <-
#'   load_data_vegetation("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_herblayer <-
#'   load_data_herblayer("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_vegetation(data_vegetation, data_herblayer)
#' }
#'
#' @param data_vegetation dataframe on vegetation with variables ...
#' @param data_herblayer dataframe on vegetation in the species level ('herb layer') with variables ...
#'
#' @return List of dataframes that are mentioned in the above description
#'
#' @export
#'
#' @importFrom dplyr %>% filter
#' @importFrom rlang .data
#'
calculate_vegetation <- function(data_vegetation, data_herblayer) {
  by_plot <- calculate_vegetation_plot(data_vegetation, data_herblayer)
  data_herblayer_CA <- data_herblayer %>%
    filter(.data$plottype == 30)
  data_vegetation_CA <- data_vegetation %>%
    filter(.data$plottype == 30)
  by_core_area_species <- calculate_vegetation_core_area_species(data_herblayer_CA, data_vegetation_CA)

  return(
    list(
      vegetation_by_plot = by_plot,
      vegetation_by_core_area_species = by_core_area_species
    )
  )
}
