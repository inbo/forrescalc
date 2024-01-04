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
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_vegetation <- load_data_vegetation(path_to_fieldmapdb)
#' data_herblayer <- load_data_herblayer(path_to_fieldmapdb)
#' calculate_vegetation(data_vegetation, data_herblayer)
#'
#' @param data_vegetation dataframe on vegetation with variables ...
#' @param data_herblayer dataframe on vegetation in the species level
#' ('herb layer') with variables ...
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
    filter(.data$plottype == "CA")
  by_core_area_species <- calculate_vegetation_core_area_species(data_herblayer_CA)

  return(
    list(
      vegetation_by_plot = by_plot,
      vegetation_by_core_area_species = by_core_area_species
    )
  )
}
