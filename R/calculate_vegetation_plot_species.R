#' give percentage presence of species in subplots
#'
#' This function calculates for each plot, species and year the percentage of subplots in which the species is present
#'
#' @inheritParams calculate_vegetation
#'
#' @return dataframe with columns plot, species, year and perc_of_subplots
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(dplyr)
#' data_vegetation_CA <-
#'   load_data_vegetation("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb") %>%
#'   filter(Plottype == 30)
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
      perc_of_subplots = n_distinct(.data$subplot_id) * 100 / 98
    ) %>%
    ungroup()

  return(by_plot_species)
}
