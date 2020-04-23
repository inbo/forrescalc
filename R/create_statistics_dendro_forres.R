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
#' @importFrom dplyr %>% group_by inner_join n n_distinct summarise ungroup
#' @importFrom rlang .data
#'
create_statistics_dendro_forres <- function(dendro_by_plot) {
  by_forres_year <- dendro_by_plot %>%
    mutate(
      species_alive = ifelse(.data$AliveDead == 11, .data$species, NA)
    ) %>%
    group_by(.data$plot_id, .data$year, .data$period) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species_alive, na.rm = TRUE),
      number_of_trees_ha =
        round(sum(.data$AliveDead == 11) / unique(.data$plotarea_ha)),
      basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
      basal_area_snag_m2_ha = sum(.data$basal_area_snag_m2_ha),
      volume_alive_m3_ha = sum(.data$volume_alive_m3_ha),
      volume_snag_m3_ha = sum(.data$volume_snag_m3_ha)
    ) %>%
    ungroup() %>%
    left_join(
      data_deadwood %>%
        group_by(.data$plot_id, .data$year, .data$period) %>%
        summarise(
          volume_log_m3_ha = sum(.data$CalcVolume_m3) / ((pi * .data$rA4 ^ 2)/10000)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period")
    ) %>%
    mutate(
      volume_deadwood_m3_ha = .data$volume_snag_m3_ha + .data$volume_log_m3_ha
    )

  return(by_forres_year)
}
