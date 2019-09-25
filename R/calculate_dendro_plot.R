#' aggregate parameters by plot
#'
#' This function compares for each plot the differences between years for: number of tree species, number of trees, basal area, and volume (calculated per hectare). It gives results for differences between subsequent measures (based on 'series') and between the last and the first measure.
#'
#' @param by_plot_year dataframe with values for each plot and year, which is the result of the calculation by function calculate_dendro_plot_year()
#'
#' @return dataframe with columns plot, number_of_tree_species, number_of_trees, basal_area, volume
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows filter group_by inner_join mutate summarise transmute ungroup
#' @importFrom rlang .data
#'
calculate_dendro_plot <- function(by_plot_year) {
  #data for comparison between 2 subsequent measures (series)
  by_plot <- by_plot_year %>%
    mutate(
      series_min = .data$series - 1
    ) %>%
    bind_rows( #data for comparison between last and first measure of plot
      by_plot_year %>%
        filter(.data$series > 2) %>%
        group_by(.data$plot_id) %>%
        summarise(series = max(.data$series)) %>%
        ungroup() %>%
        inner_join(
          by_plot_year,
          by = c("plot_id", "series")
        ) %>%
        mutate(
          series_min = 1
        )
    ) %>%  #join to earlier measure
    inner_join(
      by_plot_year,
      by = c("plot_id", "series_min" = "series"),
      suffix = c("", "_added")
    ) %>%
    transmute(  #calculate: make the comparison
      .data$plot_id,
      series_diff = paste(.data$series, .data$series_min, sep = " - "),
      year_diff = paste(.data$year, .data$year_added, sep = " - "),
      number_of_tree_species_diff =
        .data$number_of_tree_species - .data$number_of_tree_species_added,
      number_of_trees_diff =
        .data$number_of_trees - .data$number_of_trees_added,
      basal_area_diff = .data$basal_area - .data$basal_area_added,
      volume_stem_diff = .data$volume_stem - .data$volume_stem_added
    )

  return(by_plot)
}
