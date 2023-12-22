#' copy table(s) from access db to git repository
#'
#' This function loads one or more tables from the access database and saves
#' them in a git repository.
#'
#' @param tables vector with table names of tables that should be moved
#' @inheritParams load_data_dendrometry
#' @inheritParams save_results_git
#'
#' @return No value is returned, the tables are saved in the git repository.
#'
#' @export
#'
#' @importFrom git2rdata commit pull push repository write_vc
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlFetch
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("database/mdb_bosres.sqlite", package = "forrescalc")
#' from_access_to_git(
#'   database = path_to_fieldmapdb,
#'   tables = c("qLayer", "qMossLondo"),
#'   repo_path = "C:/gitrepo/forresdat"
#' )
#'
from_access_to_git <-
  function(database, tables, repo_path, push = FALSE, strict = TRUE) {
  repo <- repository(repo_path)
  pull(repo, credentials = get_cred(repo))
  con <- odbcConnectAccess2007(database)
  for (tablename in tables) {
    table <- sqlFetch(con, tablename, stringsAsFactors = FALSE)
    write_vc(table, file = paste0("data/", tablename), root = repo,
             sorting = "ID", stage = TRUE, strict = strict)
  }
  odbcClose(con)
  tryCatch(
    commit(repo, message = "scripted commit: copy from fieldmap", session = TRUE),
    error = function(e) {
      val <- withCallingHandlers(e)
      if (
        startsWith(
          val[["message"]], "Error in 'git2r_commit': Nothing added to commit"
        )
      ) {
        stop(
          "Tables in database and git-repository are identical, so no commit added", # nolint
          call. = FALSE
        )
      }
      stop(e)
    }
  )
  if (push) {
    push(repo, credentials = get_cred(repo))
  }
}
