#' Calculate statistics for the given dataset
#'
#' This function calculates statistics for the given data (e.g. from the git-repository forresdat) on the specified level (e.g. forest_reserve and species) and for the specified variables (e.g. basal_area and volume). Calculated statistics include mean, variance and confidence interval.
#'
#' @param dataset dataset with data to be summarised with at least columns year and period, e.g. table from git repository forresdat
#' @param level grouping variables that determine on which level the values should be calculated (e.g. forest_reserve and species), given as a string or a vector of strings.  Datasets are automatically grouped by year and period and it is not necessary to include these variables here.
#' @param variables variable(s) of which summary statistics should be calculated (given as a string or a vector of strings)
#'
#' @return dataframe with columns year, period and the columns chosen for level and variable
#'
#' @examples
#' \dontrun{
#' #change path before running
#' dendro_by_plot <-
#'   read_git(tablename = "dendro_by_plot", repo_path = "C:/gitrepo/forresdat")
#' create_statistics(dataset = dendro_by_plot, level = "forest_reserve", variables = "volume_alive_m3_ha")
#' dendro_by_diam_plot_species <-
#'   read_git(tablename = "dendro_by_diam_plot_species", repo_path = "C:/gitrepo/forresdat")
#' create_statistics(
#'   dataset = dendro_by_diam_plot_species,
#'   level = c("forest_reserve", "species", "dbh_class_5cm"),
#'   variables = c("basal_area_shoot_alive_m2_ha", "basal_area_shoot_snag_m2_ha",
#'       "basal_area_tree_alive_m2_ha", "basal_area_tree_snag_m2_ha")
#' )
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by_at summarise_at ungroup vars
#' @importFrom rlang .data
#'
create_statistics <- function(dataset, level = "forest_reserve", variables) {
  level <- c("year", "period", level)
  statistics <- dataset %>%
    group_by_at(vars(level)) %>%
    summarise_at(
      variables, list(n = n, mean = mean, var = var)
    ) %>%
    ungroup()

  return(statistics)
}
