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
#' database <- system.file("database/mdb_bosres.sqlite", package = "forrescalc")
#' data_dendro <- load_data_dendrometry(database)
#' data_deadwood <- load_data_deadwood(database)
#' data_shoots <- load_data_shoots(database)
#' height_model <- load_height_models("C:/bosreservaten/Hoogtemodellen/")
#' result_dendro <-
#'   calculate_dendrometry(data_dendro, data_deadwood, data_shoots, height_model)
#' save_results_access(result = result_dendro, database = "C:/db/testdb.accdb")
#' #Repeating the previous line of code will give an error, because you try to
#' #overwrite a table that was already saved in the database on the first run.
#' #To overwrite previously saved tables, use this command:
#' save_results_access(result = result_dendro, database = "C:/db/testdb.accdb", remove_tables = TRUE)
#' }
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbListTables dbRemoveTable dbWriteTable
#'
save_results_access <- function(results, database, remove_tables = FALSE) {
  con <- connect_to_database(database)
  for (tablename in names(results)) {
    if (remove_tables) {
      dbtables <- dbListTables(con)
      if (tablename %in% dbtables) {
        dbRemoveTable(con, tablename)
      }
    }
    tryCatch(
      dbWriteTable(conn = con, name = tablename, value = results[[tablename]]),
      error = function(e) {
        val <- withCallingHandlers(e)
        if (
          endsWith(
            val[["message"]],
            "exists in database, and both overwrite and append are FALSE"
          )
        ) {
          tablename <-
            sub(
              "Error\\: Table","", val[["message"]]
            )
          tablename <-
            sub(
              "exists in database, and both overwrite and append are FALSE",
              "", tablename
            )
          stop(
            paste0(
              tablename,
              "exists in database, add 'remove_tables = TRUE' to overwrite it"
            ),
            call. = FALSE
          )
        }
        stop(e)
      }
    )

  }
  dbDisconnect(con)
}
