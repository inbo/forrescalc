#' aggregates parameters on vegetation of forests
#'
#' This function makes aggregations of vegetation data on the levels of
#' \itemize{
#'  \item plot and year
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
calculate_vegetation <- function(data_vegetation) {
  by_plot <- calculate_vegetation_plot(data_vegetation)

  return(
    list(
      vegetation_by_plot = by_plot
    )
  )
}
