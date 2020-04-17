#' aggregate regeneration parameters by plot, species and year
#'
#' This function calculates for each plot, species and year the percentage of subplots in which the species is regenerating. This calculation is designed for core areas, that consist of different subplots.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, species, year, number_of_subplots_with_regeneration and perc_subplots_with_regeneration
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_regeneration_CA <-
#'   load_data_vegetation(
#'     "C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb",
#'     plottype == 30
#'   )
#' calculate_regeneration_plot_species(data_regeneration_CA)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_species <- function(data_regeneration) {
  by_plot_species <- data_regeneration %>%
    group_by(.data$plot_id, .data$year, .data$period, .data$species) %>%
    summarise(
      number_of_subplots_with_regeneration = n_distinct(.data$subplot_id),
      perc_subplots_with_regeneration =
        .data$number_of_subplots_with_regeneration * 100 / 98
    ) %>%
    ungroup()

  return(by_plot_species)
}
