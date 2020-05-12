#' load tables from git repository
#'
#' This function reads a table in git2rdata format from a git repository.
#'
#' @param tablename name of the table that should be read
#' @param repo_path name and path of local git repository from which data should be retrieved
#'
#' @return A dataframe with the specified table
#'
#' @examples
#' \dontrun{
#' #change path before running
#' read_git(tablename = "dendro_status_tree", repo_path = "C:/gitrepo/forresdat")
#' }
#'
#' @export
#'
#' @importFrom git2rdata pull read_vc repository
#'
read_git <- function(tablename, repo_path) {
  repo <- repository(repo_path)
  pull(repo, credentials = get_cred(repo))
  dataset <- read_vc(file = paste0("data/", tablename), root = repo)
  return(dataset)
}
