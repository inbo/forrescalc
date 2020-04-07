#' aggregate vegetation parameters by plot and year
#'
#' This function calculates for each plot and year the total coverage and the number of species in the vegetation layer.
#'
#' @inheritParams calculate_vegetation
#'
#' @return dataframe with columns plot, year and number_of_tree_species
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_vegetation <-
#'   load_data_vegetation("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_vegetation_plot(data_vegetation)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_vegetation_plot <- function(data_vegetation) {
  by_plot <- data_vegetation %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$total_moss_cover_id,
      .data$total_herb_cover_id, .data$total_shrub_cover_id,
      .data$total_tree_cover_id, .data$shrub_cover_min, .data$shrub_cover_max,
      .data$tree_cover_min, .data$tree_cover_max
    ) %>%
    summarise(
      number_of_species = n_distinct(.data$species)
    ) %>%
    ungroup() %>%
    mutate(
      shrub_tree_cover_min =
        100 * (1 - (1 - .data$shrub_cover_min / 100) * (1 - .data$tree_cover_min / 100)),
      shrub_tree_cover_max =
        100 * (1 - (1 - .data$shrub_cover_max / 100) * (1 - .data$tree_cover_max / 100))
    ) %>%
    select(
      -.data$shrub_cover_min, -.data$shrub_cover_max, -.data$tree_cover_min,
      -.data$tree_cover_max
    )

  return(by_plot)
}
