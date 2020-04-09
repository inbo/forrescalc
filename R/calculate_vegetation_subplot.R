#' aggregate vegetation parameters by subplot and year
#'
#' This function calculates for each subplot and year the the number of species and cumulated herb layer coverage in the vegetation layer.  This calculation is designed for core areas, that consist of different subplots.  In other plot types, this function gives the same result as calculate_vegetation_plot().
#'
#' @inheritParams calculate_vegetation
#'
#' @return dataframe with columns plot, subplot, year and number_of_species
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_vegetation <-
#'   load_data_vegetation("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb") %>%
#'   filter(plottype == 30)
#' calculate_vegetation_subplot(data_vegetation)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_vegetation_subplot <- function(data_vegetation) {
  by_subplot <- data_vegetation %>%
    group_by(.data$plot_id, .data$subplot_id, .data$year, .data$period) %>%
    summarise(
      number_of_species = n_distinct(.data$species)
    ) %>%
    ungroup()

  return(by_subplot)
}
