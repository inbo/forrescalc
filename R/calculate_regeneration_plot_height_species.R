#' aggregate parameters by plot, height class and species
#'
#' This function compares for each plot, height class and species the differences between years for the number of individuals. It gives results for differences between subsequent measures (based on 'series') and between the last and the first measure.
#'
#' @param by_plot_height_species_year dataframe with values for each plot, height class, species and year, which is the result of the calculation by function calculate_regeneration_plot_height_species_year()
#'
#' @return dataframe with columns plot, height class, species and number_of_trees_diff
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows filter group_by inner_join mutate summarise transmute ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_height_species <- function(by_plot_height_species_year) {
  #data for comparison between 2 subsequent measures (series)
  by_plot_height_species <- by_plot_height_species_year %>%
    mutate(
      series_min = .data$series - 1
    ) %>%
    bind_rows( #data for comparison between last and first measure of plot
      by_plot_height_species_year %>%
        filter(.data$series > 2) %>%
        group_by(.data$plot_id) %>%
        summarise(series = max(.data$series)) %>%
        ungroup() %>%
        inner_join(
          by_plot_height_species_year,
          by = c("plot_id", "series")
        ) %>%
        mutate(
          series_min = 1
        )
    ) %>%  #join to earlier measure
    inner_join(
      by_plot_height_species_year,
      by = c("plot_id", "height_class", "species", "series_min" = "series"),
      suffix = c("", "_added")
    ) %>%
    transmute(  #calculate: make the comparison
      .data$plot_id, .data$height_class, .data$species,
      series_diff = paste(.data$series, .data$series_min, sep = " - "),
      year_diff = paste(.data$year, .data$year_added, sep = " - "),
      min_number_of_trees_diff =
        .data$min_number_of_trees - .data$min_number_of_trees_added,
      max_number_of_trees_diff =
        .data$max_number_of_trees - .data$max_number_of_trees_added
    )

  return(by_plot_height_species)
}
