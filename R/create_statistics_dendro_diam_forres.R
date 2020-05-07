#' Calculate statistics by forest reserve, diameter and year
#'
#' This function shows data from the git-repository forresdat on the level of plot and year some values per diameter class and hectare: number of trees, basal area, and volume.
#'
#' @param dendro_by_diam_plot data on the level of plot, diameter and year, e.g. table dendro_by_diam_plot in git repository forresdat or the result of function calculate_diam_plot()
#'
#' @return dataframe with columns forres, year, number_of_trees_ha, basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' \dontrun{
#' #change path before running
#' dendro_by_diam_plot <-
#'   read_git(tablename = "dendro_by_diam_plot", repo_path = "C:/gitrepo/forresdat")
#' create_statistics_dendro_diam_forres(dendro_by_diam_plot)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by summarise ungroup
#' @importFrom rlang .data
#'
create_statistics_dendro_diam_forres <- function(dendro_by_diam_plot) {
  by_forres_diam <- dendro_by_diam_plot %>%
    group_by(
      .data$forest_reserve, .data$year, .data$period, .data$dbh_class_5cm
    ) %>%
    summarise(
      stem_number_alive_ha = mean(.data$stem_number_alive_ha),
      stem_number_snag_ha = mean(.data$stem_number_snag_ha),
      basal_area_shoot_alive_m2_ha = mean(.data$basal_area_shoot_alive_m2_ha),
      basal_area_shoot_snag_m2_ha = mean(.data$basal_area_shoot_snag_m2_ha),
      basal_area_tree_alive_m2_ha = mean(.data$basal_area_tree_alive_m2_ha),
      basal_area_tree_snag_m2_ha = mean(.data$basal_area_tree_snag_m2_ha),
      volume_tree_alive_m3_ha = mean(.data$volume_tree_alive_m3_ha),
      volume_tree_snag_m3_ha = mean(.data$volume_tree_snag_m3_ha),
      log_number_ha = mean(.data$log_number_ha),
      volume_log_m3_ha = mean(.data$volume_log_m3_ha)
    ) %>%
    ungroup()

  return(by_forres_diam)
}
