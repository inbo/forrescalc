#' save results of calculations in git repository
#'
#' This function saves the results from calculations in the forrescalc package (or any other named list with dataframes) in a git repository.  List item names will be used to name each of the tables, which contain as a content the different dataframes.
#'
#' @param results results from calculations in package forrescalc as a named list
#' @param repo_path name and path of local git repository in which results should be saved
#'
#' @return No value is returned, data are saved in the specified git repository
#'
#' @export
#'
#' @importFrom git2r commit pull push repository
#' @importFrom git2rdata write_vc
#'
save_results_git <- function(results, repo_path) {
  repo <- repository(repo_path)
  pull(repo)
  sorting_max <-
    c("period", "year", "plot_id", "tree_measure_id", "height_class", "species")
  for (tablename in names(results)) {
    sorting <- sorting_max[sorting_max %in% colnames(results[[tablename]])]
    write_vc(results[[tablename]], file = paste0("data/", tablename),
             root = repo, sorting = sorting, stage = TRUE)
    commit(repo, paste("add", tablename))
  }
  push(repo)
}
