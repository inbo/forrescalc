#' aggregate vegetation parameters by plot and year
#'
#' This function calculates for each plot (subplot in case of core area) and
#' year the total coverage and the number of species in the vegetation layer.
#' Year refers to year of the main vegetation survey
#' (source is table "data_vegetation"),
#' and will in some cases differ from the year of the spring flora survey.
#'
#' @inheritParams calculate_vegetation
#'
#' @return dataframe with columns plot, subplot, date, year (year of main
#' vegetation survey, possible deviating year of spring survey not taken into
#' account), number_of_tree_species and min/max/mid cover of the different
#' vegetation layers (moss, herb, shrub, tree), the waterlayer and since 2015
#' also of the soildisturbance by game.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_vegetation <-
#'   load_data_vegetation("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_herblayer <-
#'   load_data_herblayer("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_vegetation_plot(data_vegetation, data_herblayer)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by left_join mutate n_distinct select summarise
#' ungroup
#' @importFrom rlang .data
#'
calculate_vegetation_plot <- function(data_vegetation, data_herblayer) {
  by_plot <- data_herblayer %>%
    select(
      .data$plot_id, .data$period, .data$subplot_id, .data$species,
      .data$coverage_class_average_perc
    ) %>%
    group_by(
      .data$plottype, .data$plot_id, .data$period, .data$subplot_id
    ) %>%
    summarise(
      number_of_species = n_distinct(.data$species, na.rm = TRUE),
      cumm_herb_coverage_class_average_perc =
        sum(.data$coverage_class_average_perc) / n_distinct(.data$subplot_id)
    ) %>%
    ungroup() %>%
    left_join(
      data_vegetation %>%
        select(
          .data$plottype, .data$plot_id, .data$subplot_id, .data$period,
          .data$year_main_survey, .data$date_vegetation,
          .data$moss_cover_min, .data$moss_cover_max, .data$moss_cover_mid,
          .data$herb_cover_min, .data$herb_cover_max, .data$herb_cover_mid,
          .data$shrub_cover_min, .data$shrub_cover_max, .data$shrub_cover_mid,
          .data$tree_cover_min, .data$tree_cover_max, .data$tree_cover_mid,
          .data$waterlayer_cover_min, .data$waterlayer_cover_max,
          .data$waterlayer_cover_mid, .data$soildisturbance_game_cover_min,
          .data$soildisturbance_game_cover_max,
          .data$soildisturbance_game_cover_mid
        ),
      by = c("plottype", "plot_id", "period", "subplot_id")
    ) %>%
    mutate(
      # CCC: total cover in percentage = (TL/100 + SL/100 - TL/100 * SL/100) * 100,
      # where TL is the percentage cover of the tree layer and SL is the percentage cover of the shrub layer.
      cumulated_canopy_cover_min =
        100 * (1 - (1 - .data$shrub_cover_min / 100) * (1 - .data$tree_cover_min / 100)),
      cumulated_canopy_cover_max =
        100 * (1 - (1 - .data$shrub_cover_max / 100) * (1 - .data$tree_cover_max / 100)),
      cumulated_canopy_cover_mid =
        100 * (1 - (1 - .data$shrub_cover_mid / 100) * (1 - .data$tree_cover_mid / 100))
    )

  return(by_plot)
}
