#' aggregates parameters on dendrometry of trees
#'
#' This function makes aggregations of individual tree measures on the levels of
#' \itemize{
#'  \item plot and year
#'  \item plot, tree species and year
#'  \item individual trees: statuses in different years
#' }
#'
#' @param data_dendro dataframe on tree measures with variables ...
#'
#' @return List of dataframes that are mentioned in the above description
#'
#' @export
#'
calculate_dendrometry <- function(data_dendro) {
  by_plot_year <- calculate_dendro_plot_year(data_dendro)
  by_plot_species_year <- calculate_dendro_plot_species_year(data_dendro)
  status_tree <- summarise_status(data_dendro)

  return(
    list(
      dendro_by_plot_year = by_plot_year,
      dendro_by_plot_species_year = by_plot_species_year,
      dendro_status_tree = status_tree
    )
  )
}
