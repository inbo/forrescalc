#' copy table(s) from git repository to access db
#'
#' This function loads one or more tables from  a git repository and saves them in an access database.
#'
#' @inheritParams from_access_to_git
#' @inheritParams load_data_dendrometry
#' @inheritParams save_results_git
#'
#' @return No value is returned, the tables are saved in the access database.
#'
#' @export
#'
#' @importFrom git2rdata pull repository read_vc
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlSave
#'
#' @examples
#' \dontrun{
#' #change paths before running
#' library(forrescalc)
#' from_git_to_access(
#'   tables = c("qAliveDead", "qdecaystage"),
#'   repo_path = "C:/gitrepo/forresdat",
#'   database = "C:/db/testdb.accdb"
#' )
#' }
#'
from_git_to_access <- function(tables, repo_path, database) {
  repo <- repository(repo_path)
  pull(repo, credentials = get_cred(repo))
  con <- odbcConnectAccess2007(database)
  for (tablename in tables) {
    dataset <- read_vc(file = paste0("data/", tablename), root = repo)
    sqlSave(con, dataset, tablename = tablename)
  }
  odbcClose(con)
}
