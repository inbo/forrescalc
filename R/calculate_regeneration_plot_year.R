#' calculate species number by plot and year
#'
#' This function calculates for each plot and year the number of species for generation (for all height classes together).
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, year and number_of_tree_species
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_regeneration_plot_year(data_regeneration)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_year <- function(data_regeneration) {
  by_plot_year <- data_regeneration %>%
    group_by(.data$plot_id, .data$year, .data$period) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species)
    ) %>%
    ungroup()

  return(by_plot_year)
}
