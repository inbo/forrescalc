#' aggregate parameters by plot
#'
#' This function compares for each plot the differences between years for: number of tree species, number of trees, basal area, and volume (calculated per hectare). It gives results for differences between subsequent measures (based on 'period') and between the last and the first measure.
#'
#' @param by_plot_species_year dataframe with values for each plot, species and year, which is the result of the calculation by function calculate_dendro_plot_species_year()
#'
#' @return dataframe with columns plot, number_of_tree_species, number_of_trees, basal_area, volume
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows filter group_by inner_join mutate summarise transmute ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot_species <- function(by_plot_species_year) {
  #data for comparison between 2 subsequent measures (periods)
  by_plot_species <- by_plot_species_year %>%
    mutate(
      period_min = .data$period - 1
    ) %>%
    bind_rows( #data for comparison between last and first measure of plot
      by_plot_species_year %>%
        filter(.data$period > 2) %>%
        group_by(.data$plot_id, .data$species) %>%
        summarise(period = max(.data$period)) %>%
        ungroup() %>%
        inner_join(
          by_plot_species_year,
          by = c("plot_id", "species", "period")
        ) %>%
        mutate(
          period_min = 1
        )
    ) %>%  #join to earlier measure
    inner_join(
      by_plot_species_year,
      by = c("plot_id", "species", "period_min" = "period"),
      suffix = c("", "_added")
    ) %>%
    transmute(  #calculate: make the comparison
      .data$plot_id,
      .data$species,
      period_diff = paste(.data$period, .data$period_min, sep = " - "),
      year_diff = paste(.data$year, .data$year_added, sep = " - "),
      number_of_trees_ha_diff =
        .data$number_of_trees_ha - .data$number_of_trees_ha_added,
      basal_area_ha_diff = .data$basal_area_ha - .data$basal_area_ha_added,
      volume_stem_ha_diff = .data$volume_stem_ha - .data$volume_stem_ha_added
    )

  return(by_plot_species)
}
