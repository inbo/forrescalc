#' aggregates parameters on regeneration of trees
#'
#' This function makes aggregations of tree generation data on the levels of
#' \itemize{
#'  \item plot, height class and year
#'  \item plot and year
#'  \item plot, height class, tree species and year
#' }
#'
#' @param data_regeneration dataframe on tree regeneration with variables ...
#'
#' @return List of dataframes that are mentioned in the above description
#'
#' @export
#'
calculate_regeneration <- function(data_regeneration) {
  by_plot_height_year <-
    calculate_regeneration_plot_height_year(data_regeneration)
  by_plot_year <-
    calculate_regeneration_plot_year(data_regeneration)
  by_plot_height_species_year <-
    calculate_regeneration_plot_height_species_year(data_regeneration)

  return(
    list(
      regeneration_by_plot_height_year = by_plot_height_year,
      regeneration_by_plot_year = by_plot_year,
      regeneration_by_plot_height_species_year = by_plot_height_species_year
    )
  )
}
