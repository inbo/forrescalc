#' aggregate vegetation parameters by plot, species and year
#'
#' This function calculates for each plot, species and year the percentage of subplots in which the species is present and the percentage of subplots where the species is browsed (relative to the plots where it is present).  A difference is made between browsed (which contains all damage) and seriously browsed, which is reported if the damage is more than 1/20.  This calculation is designed for core areas, that consist of different subplots.
#'
#' @inheritParams calculate_vegetation
#'
#' @return dataframe with columns plot, species, year, perc_of_subplots, perc_of_subplots_browsed and perc_of_subplots_seriously_browsed
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(dplyr)
#' data_vegetation_CA <-
#'   load_data_vegetation("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb") %>%
#'   filter(plottype == 30)
#' calculate_vegetation_plot_species(data_vegetation_CA)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_vegetation_plot_species <- function(data_vegetation) {
  by_plot_species <- data_vegetation %>%
    group_by(.data$plot_id, .data$year, .data$period, .data$species) %>%
    summarise(
      number_of_subplots = n_distinct(.data$subplot_id),
      perc_of_subplots = .data$number_of_subplots * 100 / 98,
      number_of_subplots_browsed =
        sum(!is.na(.data$browse_index_id) & .data$browse_index_id %in% c(10, 20)),
      number_of_subplots_seriously_browsed =
        sum(!is.na(.data$browse_index_id) & .data$browse_index_id == 20),
      perc_of_subplots_browsed = .data$number_of_subplots_browsed * 100 / .data$number_of_subplots,
      perc_of_subplots_seriously_browsed = .data$number_of_subplots_seriously_browsed * 100 / .data$number_of_subplots
    ) %>%
    ungroup()

  return(by_plot_species)
}
