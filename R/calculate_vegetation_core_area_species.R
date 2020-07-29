#' aggregate vegetation parameters by core area, species and year
#'
#' This function calculates for each plot, species and year the percentage of subplots in which the species is present and the percentage of subplots where the species is browsed (relative to the plots where it is present).  A difference is made between browsed (which contains all damage) and seriously browsed, which is reported if the damage is more than 1/20.  This calculation is designed for core areas, that consist of different subplots. Year refers to year of recording of that specific species (source is table "data_herblayer"), and is possibly different for spring flora than for other species in the same subplot.
#'
#' @inheritParams calculate_vegetation
#'
#' @return dataframe with columns plot, species, year (year of recording of specific species, possibly different for spring flora), number_of_subplots (= number of subplots where the species occurs), perc_of_subplots (= percentage of subplots with species), number_of_subplots_browsed, perc_of_subplots_browsed, number_of_subplots_seriously_browsed, perc_of_subplots_seriously_browsed and mean_coverage_class_average_perc
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_herblayer_CA <-
#'   load_data_herblayer(
#'     "C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb",
#'     plottype = "Core area"
#'   )
#' calculate_vegetation_core_area_species(data_herblayer_CA)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_vegetation_core_area_species <- function(data_herblayer) {
  by_core_area_species <- data_herblayer %>%
    mutate(
      n_subplots = n_distinct(.data$subplot_id)
    ) %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$species
    ) %>%
    summarise(
      number_of_subplots_with_vegetation = n_distinct(.data$subplot_id),
      perc_of_subplots = .data$number_of_subplots_with_vegetation * 100 / unique(.data$n_subplots),
      number_of_subplots_browsed =
        ifelse(
          all(is.na(.data$browse_index_id)),
          NA,
          sum(!is.na(.data$browse_index_id) &
                .data$browse_index_id %in% c(10, 20))
        ),
      number_of_subplots_seriously_browsed =
        ifelse(
          all(is.na(.data$browse_index_id)),
          NA,
          sum(!is.na(.data$browse_index_id) & .data$browse_index_id == 20)
        ),
      perc_of_subplots_browsed =
        .data$number_of_subplots_browsed * 100 / .data$number_of_subplots_with_vegetation,
      perc_of_subplots_seriously_browsed =
        .data$number_of_subplots_seriously_browsed * 100 / .data$number_of_subplots_with_vegetation,
      mean_coverage_class_average_perc = mean(.data$coverage_class_average_perc)
    ) %>%
    ungroup()

  return(by_core_area_species)
}
