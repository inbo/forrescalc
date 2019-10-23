#' save results of calculations in git repository
#'
#' This function saves the results from calculations in the forrescalc package (or any other named list with dataframes) in a git repository.  List item names will be used to name each of the tables, which contain as a content the different dataframes.
#'
#' @param results results from calculations in package forrescalc as a named list
#' @param repo_path name and path of local git repository in which results should be saved
#' @param push push commits directly to the remote on github? Default is FALSE (no). (This option can only be used with SSH.)
#'
#' @return No value is returned, data are saved in the specified git repository
#'
#' @export
#'
#' @importFrom git2rdata commit pull push repository write_vc
#'
save_results_git <- function(results, repo_path, push = FALSE) {
  repo <- repository(repo_path)
  pull(repo)
  sorting_max <-
    c("period", "year", "plot_id", "tree_measure_id", "height_class", "species")
  for (tablename in names(results)) {
    sorting <- sorting_max[sorting_max %in% colnames(results[[tablename]])]
    write_vc(results[[tablename]], file = paste0("data/", tablename),
             root = repo, sorting = sorting, stage = TRUE)
  }
  commit(repo, message = "scripted commit from forrescalc", session = TRUE)
  if (push) {
    push(repo, credentials = get_cred())
  }
}
