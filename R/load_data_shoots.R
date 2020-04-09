#' retrieve data on shoots from fieldmap database
#'
#' This function queries the given database to retrieve additional data on shoots to use with dendrometry data (for functions on calculations of diameters).
#'
#' @param database name of fieldmap/access database (with specific fieldmap structure) including path
#'
#' @return Dataframe with shoot data
#'
#' @examples
#' \dontrun{
#' #change path before running
#' load_data_shoots("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows mutate
#'
load_data_shoots <- function(database) {
  query_shoots <-
    "SELECT Shoots.IDPlots AS plot_id,
      Shoots.IDTrees AS tree_measure_id,
      Shoots.DBH_mm AS dbh_mm,
      Shoots.Height_m,
      Shoots.DecayStage_Shoots as decaystage
    FROM Shoots;"

  query_shoots2 <-
    "SELECT Shoots.IDPlots AS plot_id,
      Shoots.IDTrees_2eSet AS tree_measure_id,
      Shoots.DBH_mm AS dbh_mm,
      Shoots.Height_m,
      Shoots.DecayStage_shoots as decaystage
    FROM Shoots_2eSet Shoots;"

  con <- odbcConnectAccess2007(database)
  data_shoots <- sqlQuery(con, query_shoots, stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_shoots2, stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    )
  odbcClose(con)

  return(data_shoots)
}
