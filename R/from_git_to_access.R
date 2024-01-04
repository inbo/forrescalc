#' copy table(s) from git repository to access db
#'
#' This function loads one or more tables from  a git repository and saves them
#' in an access database.
#'
#' @inheritParams from_access_to_git
#' @inheritParams load_data_dendrometry
#' @inheritParams save_results_git
#' @inheritParams save_results_access
#'
#' @return No value is returned, the tables are saved in the access database.
#'
#' @export
#'
#' @importFrom git2rdata pull repository read_vc
#' @importFrom DBI dbDisconnect
#'
#' @examples
#' \dontrun{
#' #change paths before running
#' library(forrescalc)
#' # (add path to your own database here)
#' path_to_database <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' from_git_to_access(
#'   tables = c("qAliveDead", "qdecaystage"),
#'   repo_path = "C:/gitrepo/forresdat",
#'   database = path_to_database,
#'   remove_tables = TRUE
#' )
#' }
#'
from_git_to_access <-
  function(tables, repo_path, database, remove_tables = FALSE) {
  repo <- repository(repo_path)
  pull(repo, credentials = get_cred(repo))
  con <- connect_to_database(database)
  for (tablename in tables) {
    dataset <- read_vc(file = paste0("data/", tablename), root = repo)
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
