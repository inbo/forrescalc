#' save results of calculations in Access database
#'
#' This function saves the results from calculations in the forrescalc package (or any other named list with dataframes) in an Access database.  List item names will be used to name each of the tables, which contain as a content the different dataframes.
#'
#' @param results results from calculations in package forrescalc as a named list
#' @param database name of (empty) Access database including path in which results should be saved
#'
#' @return No value is returned, data are saved in the specified database
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlSave
#'
save_results_access <- function(results, database) {
  con <- odbcConnectAccess2007(database)
  for (tablename in names(results)) {
    sqlSave(con, results[[tablename]], tablename = tablename)
  }
  odbcClose(con)
}
