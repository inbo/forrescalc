#' Calculate statistics by forest reserve and year
#'
#' This function shows data from the git-repository forresdat on the level of plot and year some values per hectare: number of tree species, number of trees, basal area, and volume.
#'
#' @param dendro_by_plot data on the level of plot and year, e.g. table dendro_by_plot in git repository forresdat or the result of function calculate_dendro_plot()
#'
#' @return dataframe with columns forres, year, number_of_tree_species, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' dendro_by_plot <-
#'   read_git(tablename = "dendro_by_plot", repo_path = "C:/gitrepo/forresdat")
#' create_statistics_dendro_forres(dendro_by_plot)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by summarise ungroup
#' @importFrom rlang .data
#'
create_statistics_dendro_forres <- function(dendro_by_plot) {
  by_forres <- dendro_by_plot %>%
    group_by(.data$forest_reserve, .data$year, .data$period) %>%
    summarise(
      number_of_tree_species = mean(.data$number_of_tree_species),
      number_of_trees_ha = mean(.data$number_of_trees_ha),
      stem_number_ha = mean(.data$stem_number_ha),
      basal_area_alive_m2_ha = mean(.data$basal_area_alive_m2_ha),
      basal_area_snag_m2_ha = mean(.data$basal_area_snag_m2_ha),
      volume_alive_m3_ha = mean(.data$volume_alive_m3_ha),
      volume_snag_m3_ha = mean(.data$volume_snag_m3_ha),
      vol_stem_alive_m3_ha = mean(.data$vol_stem_alive_m3_ha),
      vol_stem_snag_m3_ha = mean(.data$vol_stem_snag_m3_ha),
      volume_log_m3_ha = mean(.data$volume_log_m3_ha),
      volume_deadwood_m3_ha = mean(.data$volume_deadwood_m3_ha),
      stems_per_tree = mean(.data$stems_per_tree)
    ) %>%
    ungroup()

  return(by_forres)
}
