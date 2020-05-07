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
#' @importFrom dplyr %>% group_by summarise ungroup
#' @importFrom rlang .data
#'
create_statistics <- function(dataset) {
  statistics <- dataset %>%
    # group_by(.data$forest_reserve, .data$year, .data$period) %>%
    # summarise(
    #   number_of_tree_species = mean(.data$number_of_tree_species),
    #   number_of_trees_ha = mean(.data$number_of_trees_ha),
    #   stem_number_ha = mean(.data$stem_number_ha),
    #   basal_area_alive_m2_ha = mean(.data$basal_area_alive_m2_ha),
    #   basal_area_snag_m2_ha = mean(.data$basal_area_snag_m2_ha),
    #   volume_alive_m3_ha = mean(.data$volume_alive_m3_ha),
    #   volume_snag_m3_ha = mean(.data$volume_snag_m3_ha),
    #   vol_stem_alive_m3_ha = mean(.data$vol_stem_alive_m3_ha),
    #   vol_stem_snag_m3_ha = mean(.data$vol_stem_snag_m3_ha),
    #   volume_log_m3_ha = mean(.data$volume_log_m3_ha),
    #   volume_deadwood_m3_ha = mean(.data$volume_deadwood_m3_ha),
    #   stems_per_tree = mean(.data$stems_per_tree)
    # ) %>%
    ungroup()

  return(statistics)
}
