#' copy table(s) from git repository forresdat to access db
#'
#' This function loads one or more tables from git repository forresdat
#' and saves them in an Access (or SQLite) database.
#'
#' @inheritParams save_results_access
#' @inheritParams from_access_to_forresdat
#' @inheritParams load_data_dendrometry
#' @inheritParams read_forresdat_table
#'
#' @return No value is returned, the tables are saved in the access database.
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbWriteTable
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own database here)
#' path_to_database <- "my-db.sqlite"
#' from_forresdat_to_access(
#'   tables = "dendro_by_plot",
#'   database = path_to_database
#' )
#' # if tables don't contain column plot_id, or it is not relevant to add
#' # information on the plots, add argument join_plotinfo = FALSE
#' from_forresdat_to_access(
#'   tables = c("qAliveDead", "qdecaystage"),
#'   database = path_to_database,
#'   join_plotinfo = FALSE
#' )
#'
#' file.remove("my-db.sqlite")
#'
from_forresdat_to_access <-
  function(
    tables, database, remove_tables = FALSE, plottype = NA, join_plotinfo = TRUE
  ) {
  if (is.na(plottype)) {
    plottype <- "all"
  }
  con <- connect_to_database(database)
  for (tablename in tables) {
    dataset <-
      read_forresdat_table(
        tablename, join_plotinfo = join_plotinfo, plottype = plottype
      )
    tryCatch(
      dbWriteTable(
        conn = con, name = tablename, value = dataset, overwrite = remove_tables
      ),
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
