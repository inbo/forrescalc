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
#' library(forrescalc)
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
      Shoots.IDTrees%2$s AS tree_measure_id,
      Shoots.ID AS shoot_measure_id,
      Shoots.DBH_mm AS dbh_mm,
      Shoots.Height_m AS height_m,
      Shoots.DecayStage_Shoots as decaystage
    FROM Shoots%2$s Shoots;"

  con <- odbcConnectAccess2007(database)
  data_shoots <- sqlQuery(con, sprintf(query_shoots, 1, ""), stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, sprintf(query_shoots, 2, "_2eSet"), stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    ) %>%
    bind_rows(
      sqlQuery(con, sprintf(query_shoots, 3, "_3eSet"), stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    )
  odbcClose(con)

  return(data_shoots)
}
