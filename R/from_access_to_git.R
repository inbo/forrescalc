#' copy table(s) from access db to git repository
#'
#' This function loads one or more tables from the access database and saves them in a git repository.
#'
#' @param tables vector with table names of tables that should be moved
#' @inheritParams load_data_dendrometry
#' @inheritParams save_results_git
#'
#' @return No value is returned, the tables are saved in the git repository.
#'
#' @export
#'
#' @importFrom git2r commit pull push repository
#' @importFrom git2rdata write_vc
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlFetch
#'
from_access_to_git <- function(database, tables, repo_path) {
  repo <- repository(repo_path)
  pull(repo)
  con <- odbcConnectAccess2007(database)
  for (tablename in tables) {
    table <- sqlFetch(con, tablename, stringsAsFactors = FALSE)
    write_vc(table, file = paste0("data/", tablename), root = repo,
             sorting = "ID", stage = TRUE)
    commit(repo, paste("add", tablename))
  }
  odbcClose(con)
  push(repo)
}
