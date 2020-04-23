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
#' data_vegetation <-
#'   load_data_vegetation("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_vegetation(data_vegetation)
#' }
#'
#' @param data_vegetation dataframe on vegetation with variables ...
#'
#' @return List of dataframes that are mentioned in the above description
#'
#' @export
#'
#' @importFrom dplyr %>% filter
#' @importFrom rlang .data
#'
calculate_vegetation <- function(data_vegetation) {
  by_plot <- calculate_vegetation_plot(data_vegetation)
  data_vegetation_CA <- data_vegetation %>%
    filter(.data$plottype == 30)
  by_subplot <- calculate_vegetation_subplot(data_vegetation_CA)
  by_plot_species <- calculate_vegetation_plot_species(data_vegetation_CA)

  return(
    list(
      vegetation_by_plot = by_plot,
      vegetation_by_subplot = by_subplot,
      vegetation_by_plot_species = by_plot_species
    )
  )
}
