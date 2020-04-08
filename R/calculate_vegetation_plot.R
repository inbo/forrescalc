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
    group_by(.data$plot_id, .data$year, .data$period) %>%
    summarise(
      moss_cover_min = mean(.data$moss_cover_min, na.rm = TRUE),
      moss_cover_max = mean(.data$moss_cover_max, na.rm = TRUE),
      herb_cover_min = mean(.data$herb_cover_min, na.rm = TRUE),
      herb_cover_max = mean(.data$herb_cover_max, na.rm = TRUE),
      shrub_cover_min = mean(.data$shrub_cover_min, na.rm = TRUE),
      shrub_cover_max = mean(.data$shrub_cover_max, na.rm = TRUE),
      tree_cover_min = mean(.data$tree_cover_min, na.rm = TRUE),
      tree_cover_max = mean(.data$tree_cover_max, na.rm = TRUE),
      waterlayer_cover_min = mean(.data$waterlayer_cover_min, na.rm = TRUE),
      waterlayer_cover_max = mean(.data$waterlayer_cover_max, na.rm = TRUE),
      number_of_species = n_distinct(.data$species)
    ) %>%
    ungroup() %>%
    mutate(
      cumulated_canopy_cover_min =
        100 * (1 - (1 - .data$shrub_cover_min / 100) * (1 - .data$tree_cover_min / 100)),
      cumulated_canopy_cover_max =
        100 * (1 - (1 - .data$shrub_cover_max / 100) * (1 - .data$tree_cover_max / 100))
    )

  return(by_plot)
}
