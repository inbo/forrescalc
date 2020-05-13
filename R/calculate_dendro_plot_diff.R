#' aggregate parameters by plot
#'
#' This function compares for each plot the differences between years for: number of tree species, number of trees, basal area, and volume (calculated per hectare). It gives results for differences between subsequent measures (based on 'period') and between the last and the first measure.
#'
#' @param dendro_by_plot dataframe with values for each plot and year, which is the result of the calculation by function calculate_dendro_plot() and can be retrieved from forresdat
#'
#' @return dataframe with columns plot, year_diff, number_of_tree_species_diff, number_of_trees_ha_diff, basal_area_m2_ha_diff, volume_stem_m3_ha_diff
#'
#' @examples
#' \dontrun{
#' #change path before running
#' dendro_by_plot <-
#'   read_git(tablename = "dendro_by_plot", repo_path = "C:/gitrepo/forresdat")
#' calculate_dendro_plot_diff(dendro_by_plot)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% mutate select transmute
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'
calculate_dendro_plot_diff <- function(dendro_by_plot) {
  #data from long to wide
  by_plot_diff <- dendro_by_plot %>%
    select(
      -.data$volume_stem_alive_m3_ha, -.data$volume_stem_snag_m3_ha,
      -.data$stem_number_ha, -.data$stems_per_tree
    ) %>%
    mutate(
      number_of_tree_species =
        replace(
          .data$number_of_tree_species,
          is.na(.data$number_of_tree_species) & !is.na(.data$year),
          0
        ),
      number_of_trees_ha =
        replace(
          .data$number_of_trees_ha,
          is.na(.data$number_of_trees_ha) & !is.na(.data$year),
          0
        ),
      basal_area_alive_m2_ha =
        replace(
          .data$basal_area_alive_m2_ha,
          is.na(.data$basal_area_alive_m2_ha) & !is.na(.data$year),
          0
        ),
      basal_area_snag_m2_ha =
        replace(
          .data$basal_area_snag_m2_ha,
          is.na(.data$basal_area_snag_m2_ha) & !is.na(.data$year),
          0
        ),
      volume_alive_m3_ha =
        replace(
          .data$volume_alive_m3_ha,
          is.na(.data$volume_alive_m3_ha) & !is.na(.data$year),
          0
        ),
      volume_snag_m3_ha =
        replace(
          .data$volume_snag_m3_ha,
          is.na(.data$volume_snag_m3_ha) & !is.na(.data$year),
          0
        ),
      volume_log_m3_ha =
        replace(
          .data$volume_log_m3_ha,
          is.na(.data$volume_log_m3_ha) & !is.na(.data$year),
          0
        ),
      volume_deadwood_m3_ha =
        replace(
          .data$volume_deadwood_m3_ha,
          is.na(.data$volume_deadwood_m3_ha) & !is.na(.data$year),
          0
        )
    ) %>%
    pivot_wider(
      names_from = "period",
      values_from =
        c(.data$year, .data$number_of_tree_species, .data$number_of_trees_ha,
          .data$basal_area_alive_m2_ha, .data$basal_area_snag_m2_ha,
          .data$volume_alive_m3_ha, .data$volume_snag_m3_ha,
          .data$volume_log_m3_ha, .data$volume_deadwood_m3_ha
          )
    ) %>%
    transmute(  #calculate: make the comparison
      .data$forest_reserve,
      .data$plot_id,
      period_diff = "2 - 1",
      year_diff = paste(.data$year_2, .data$year_1, sep = " - "),
      n_years = .data$year_2 - .data$year_1,
      number_of_tree_species_diff =
        .data$number_of_tree_species_2 - .data$number_of_tree_species_1,
      number_of_trees_ha_diff =
        .data$number_of_trees_ha_2 - .data$number_of_trees_ha_1,
      basal_area_alive_m2_ha_diff =
        .data$basal_area_alive_m2_ha_2 - .data$basal_area_alive_m2_ha_1,
      basal_area_snag_m2_ha_diff =
        .data$basal_area_snag_m2_ha_2 - .data$basal_area_snag_m2_ha_1,
      volume_alive_m3_ha_diff =
        .data$volume_alive_m3_ha_2 - .data$volume_alive_m3_ha_1,
      volume_snag_m3_ha_diff =
        .data$volume_snag_m3_ha_2 - .data$volume_snag_m3_ha_1,
      volume_log_m3_ha_diff =
        .data$volume_log_m3_ha_2 - .data$volume_log_m3_ha_1,
      volume_deadwood_m3_ha_diff =
        .data$volume_deadwood_m3_ha_2 - .data$volume_deadwood_m3_ha_1
    )

  return(by_plot_diff)
}
