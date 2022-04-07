#' remove last local change from git repository
#'
#' This function removes the last commit from the active branch of the
#' specified git repository. ONLY USE THIS FUNCTION IF YOUR COMMIT IS
#' NOT YET PUSHED TO THE REMOTE!!! This function is meant for users that
#' are not familiar with Git to easily remove an automatically generated
#' commit in forresdat after they discoved mistakes in it.
#'
#' @param repo_path name and path of local git repository in which last
#' commits should be removed
#'
#' @return A dataframe with the specified table
#'
#' @examples
#' \dontrun{
#' #change paths before running
#' library(forrescalc)
#' from_access_to_git(
#'   database = "C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb",
#'   tables = c("qAliveDead", "qdecaystage"),
#'   repo_path = "C:/gitrepo/forresdat"
#' )
#' remove_last_commit_git(repo_path = "C:/gitrepo/forresdat")
#' }
#'
#' @export
#'
#' @importFrom git2r commits reset
#' @importFrom git2rdata repository
#'
remove_last_commit_git <- function(repo_path) {
  repo <- repository(repo_path)
  reset(commits(repo)[[2]], reset_type = "hard")
}
