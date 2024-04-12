#' remove table(s) from data package forresdat
#'
#' This function removes one or more tables from the data package forresdat
#' by making a commit on a local clone of the git repository.
#' While removing the table(s), it also updates the metadata (`.json` file)
#'
#' @param tables vector with table names of tables that should be removed
#' @param repo_path name and path of local forresdat repository in which
#' results/tables should be saved
#' @inheritParams save_results_git
#'
#' @return No value is returned, the tables are removed from the git repository.
#'
#' @export
#'
#' @importFrom assertthat has_name
#' @importFrom git2r add branches checkout commit pull push repository
#' @importFrom frictionless read_package remove_resource resources write_package
#'
#' @examples
#' \dontrun{
#' #make a local clone of forresdat and change path before running
#' library(forrescalc)
#' # add path to your local clone of forresdat
#' path_to_forresdat <- "xxx/forresdat"
#' # if you don't have a local clone yet, make it:
#' git2r::clone("https://github.com/inbo/forresdat.git", path_to_forresdat)
#'
#' remove_table_forresdat(
#'   tables = c("qCoverHerbs", "qtotalCover"),
#'   repo_path = path_to_forresdat
#' )
#' }
#'
remove_table_forresdat <-
  function(tables, repo_path, push = FALSE, branch = "develop") {
  repo <- repository(repo_path)
  if (!has_name(branches(repo), branch)) {
    stop(
      sprintf(
        "Branch %s doesn't exist in forresdat. Add this branch and try again",
        branch
      )
    )
  }
  checkout(repo, branch)
  pull(repo, credentials = get_cred(repo))
  package <- read_package(file.path(repo_path, "data", "datapackage.json"))
  for (tablename in tables) {
    if (tablename %in% resources(package)) {
      package <- package |>
        remove_resource(tablename)
      file.remove(file.path(repo_path, "data", paste0(tablename, ".csv")))
    } else {
      warning(
        sprintf(
          "Table %s is not present in forresdat and cannot be removed",
          tablename
        )
      )
    }
  }
  write_package(package, file.path(repo_path, "data"))
  add(repo, path = "*")
  tryCatch(
    commit(
      repo, message = "scripted commit: table(s) removed", session = TRUE
    ),
    error = function(e) {
      val <- withCallingHandlers(e)
      if (
        startsWith(
          val[["message"]], "Error in 'git2r_commit': Nothing added to commit"
        )
      ) {
        stop(
          "Table(s) was/were not present in forresdat, so no commit is added",
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
