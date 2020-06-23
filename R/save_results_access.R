#' save results of calculations in Access database
#'
#' This function saves the results from calculations in the forrescalc package (or any other named list with dataframes) in an Access database.  List item names will be used to name each of the tables, which contain as a content the different dataframes.
#'
#' @param results results from calculations in package forrescalc as a named list
#' @param database name of (empty) Access database including path in which results should be saved
#' @param remove_tables overwrite existing tables in database?  Default is
#' FALSE, which means tables are not overwritten/deleted unless this parameter
#' is explicitly put on TRUE.
#'
#' @return No value is returned, data are saved in the specified database
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_deadwood <-
#'   load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' result_dendro <- calculate_dendrometry(data_dendro, data_deadwood)
#' save_results_access(result = result_dendro, database = "C:/db/testdb.accdb")
#' #Repeating the previous line of code will give an error, because you try to
#' #overwrite a table that was already saved in the database on the first run.
#' #To overwrite previously saved tables, use this command:
#' save_results_access(result = result_dendro, database = "C:/db/testdb.accdb", remove_tables = TRUE)
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlDrop sqlSave sqlTables
#'
save_results_access <- function(results, database, remove_tables = FALSE) {
  con <- odbcConnectAccess2007(database)
  for (tablename in names(results)) {
    if (remove_tables) {
      dbtables <- sqlTables(con)
      if (tablename %in% dbtables$TABLE_NAME) {
        sqlDrop(con, tablename)
      }
    }
    sqlSave(con, results[[tablename]], tablename = tablename)
  }
  odbcClose(con)
}
