#' aggregate parameters by plot, height class and species
#'
#' This function compares for each plot, height class and species the differences between years for the number of individuals per hectare. It gives results for differences between subsequent measures (based on 'period') and between the last and the first measure.
#'
#' @param by_plot_height_species_year dataframe with values for each plot, height class, species and year, which is the result of the calculation by function calculate_regeneration_plot_height_species_year()
#'
#' @return dataframe with columns plot, height class, species and number_of_trees_ha_diff
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' by_plot_height_species_year <-
#'   calculate_regeneration_plot_height_species_year(data_regeneration)
#' calculate_regeneration_plot_height_species(by_plot_height_species_year)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows filter group_by inner_join mutate summarise transmute ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_height_species <- function(by_plot_height_species_year) {
  #data for comparison between 2 subsequent measures (periods)
  by_plot_height_species <- by_plot_height_species_year %>%
    mutate(
      period_min = .data$period - 1
    ) %>%
    bind_rows( #data for comparison between last and first measure of plot
      by_plot_height_species_year %>%
        filter(.data$period > 2) %>%
        group_by(.data$plot_id) %>%
        summarise(period = max(.data$period)) %>%
        ungroup() %>%
        inner_join(
          by_plot_height_species_year,
          by = c("plot_id", "period")
        ) %>%
        mutate(
          period_min = 1
        )
    ) %>%  #join to earlier measure
    inner_join(
      by_plot_height_species_year,
      by = c("plot_id", "height_class", "species", "period_min" = "period"),
      suffix = c("", "_added")
    ) %>%
    transmute(  #calculate: make the comparison
      .data$plot_id, .data$height_class, .data$species,
      period_diff = paste(.data$period, .data$period_min, sep = " - "),
      year_diff = paste(.data$year, .data$year_added, sep = " - "),
      min_number_of_trees_ha_diff =
        .data$min_number_of_trees_ha - .data$min_number_of_trees_ha_added,
      max_number_of_trees_ha_diff =
        .data$max_number_of_trees_ha - .data$max_number_of_trees_ha_added
    )

  return(by_plot_height_species)
}
