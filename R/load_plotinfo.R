#' retrieve generic plot data from fieldmap database
#'
#' This function queries the given database to retrieve additional data on plots
#' to save in forresdat and link with the datasets that are saved there.
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#'
#' @return Dataframe with columns plot_id, plottype and forest_reserve
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' load_plotinfo("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom dplyr %>% bind_rows distinct
#'
load_plotinfo <- function(database) {
  query_plot <-
    "SELECT Plots.ID AS plot_id,
      Plots.Plottype AS plottype,
      pd.ForestReserve AS forest_reserve
    FROM Plots INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots;"

  query_plot2 <-
    "SELECT Plots.ID AS plot_id,
      Plots.Plottype AS plottype,
      pd.ForestReserve AS forest_reserve
    FROM Plots INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots;"

  con <- odbcConnectAccess2007(database)
  plotinfo <- sqlQuery(con, query_plot, stringsAsFactors = FALSE) %>%
    bind_rows(
      sqlQuery(con, query_plot2, stringsAsFactors = FALSE)
    ) %>%
    distinct()
  odbcClose(con)

  return(plotinfo)
}
