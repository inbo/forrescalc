#' save results of calculations in Access database
#'
#' This function saves the results from calculations in the forrescalc package
#' (or any other named list with dataframes) in an Access database.
#' List item names will be used to name each of the tables,
#' which contain as a content the different dataframes.
#'
#' @param results results from calculations in package forrescalc as a
#'   named list
#' @param database name of (empty) Access database including path in which
#' results should be saved
#' @param remove_tables overwrite existing tables in database?  Default is
#' FALSE, which means tables are not overwritten/deleted unless this parameter
#' is explicitly put on TRUE.
#'
#' @return No value is returned, data are saved in the specified database
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' # (add path to your height models here)
#' path_to_height_models <-
#'   system.file("example/height_models", package = "forrescalc")
#'
#' # do calculations
#' data_dendro <- load_data_dendrometry(path_to_fieldmapdb)
#' data_deadwood <- load_data_deadwood(path_to_fieldmapdb)
#' data_shoots <- load_data_shoots(path_to_fieldmapdb)
#' height_model <- load_height_models(path_to_height_models)
#' plotinfo <- load_plotinfo(path_to_fieldmapdb)
#' result_dendro <-
#'   calculate_dendrometry(
#'     data_dendro, data_deadwood, data_shoots, height_model, plotinfo)
#'
#' # save the results
#' save_results_access(result = result_dendro, database = path_to_fieldmapdb)
#' # Repeating the previous line of code will give an error, because you try to
#' # overwrite a table that was already saved in the database on the first run.
#' # To overwrite previously saved tables, use this command:
#' save_results_access(
#'   result = result_dendro, database = path_to_fieldmapdb, remove_tables = TRUE
#' )
#' # To remove the tables again in the example database (undo the changes),
#' # use this code:
#' con <- forrescalc:::connect_to_database(path_to_fieldmapdb)
#' for (tablename in names(result_dendro)) {
#'   DBI::dbRemoveTable(con, tablename)
#' }
#' DBI::dbDisconnect(con)
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbListTables dbRemoveTable dbWriteTable
#'
save_results_access <- function(results, database, remove_tables = FALSE) {
  con <- connect_to_database(database)
  for (tablename in names(results)) {
    tryCatch(
      dbWriteTable(conn = con, name = tablename, value = results[[tablename]],
                   overwrite = remove_tables),
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
              "Error\\: Table", "", val[["message"]]
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
