#' remove last local change from git repository forresdat
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
#' # add path to your local clone of forresdat
#' path_to_forresdat <- "xxx/forresdat"
#'
#' # only run this after writing a commit with `save_results_forresdat()` or
#' # `from_access_to_forresdat()` that has not yet been pushed to Github!
#' remove_last_commit_forresdat(repo_path = path_to_forresdat)
#' }
#'
#' @export
#'
#' @importFrom git2r commits repository reset
#'
remove_last_commit_forresdat <- function(repo_path) {
  repo <- repository(repo_path)
  reset(commits(repo)[[2]], reset_type = "hard")
}
