#' retrieve data on shoots from fieldmap database
#'
#' This function queries the given database to retrieve additional data on shoots to use with dendrometry data.
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
#' @importFrom DBI dbDisconnect dbGetQuery
#'
load_data_shoots <- function(database) {
  query_shoots <-
    "SELECT Shoots.IDPlots AS plot_id,
      Shoots.IDTrees%2$s AS tree_measure_id,
      Shoots.ID AS shoot_measure_id,
      Shoots.DBH_mm AS dbh_mm,
      Shoots.Height_m AS height_m,
      Shoots.IntactSnag AS intact_snag,
      Shoots.DecayStage_Shoots AS decaystage
    FROM Shoots%2$s Shoots;"

  query_shoots_1986 <-
    "SELECT Shoots.IDPlots AS plot_id,
      Shoots.IDTrees_1986 AS tree_measure_id,
      Shoots.ID AS shoot_measure_id,
      Shoots.DBH_mm AS dbh_mm,
      Shoots.Height_m AS height_m,
      Shoots.IntactSnag AS intact_snag,
      Shoots.DecayStage_Shoots AS decaystage
    FROM Shoots_1986 Shoots;"

  con <- connect_to_database(database)
  shoots_1986 <- dbGetQuery(con, query_shoots_1986) %>%
    mutate(period = 0)
  dbDisconnect(con)

  data_shoots <- query_database(database, query_shoots)

  if (nrow(shoots_1986) > 0) {
    data_shoots <- data_shoots %>%
      bind_rows(
        shoots_1986
      )
  }

  data_shoots <- data_shoots %>%
    mutate(intact_snag = ifelse(is.na(.data$intact_snag), 11, .data$intact_snag))

  return(data_shoots)
}
