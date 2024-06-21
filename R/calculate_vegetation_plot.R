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
#' @return dataframe with columns `plot`, `subplot`, `date`, `year` (year of
#' main vegetation survey, possible deviating year of spring survey not taken
#' into account), `number_of_tree_species` and min/max/mid cover of the
#' different vegetation layers (moss, herb, shrub, tree), the `waterlayer` and
#' since 2015 also of the `soildisturbance` by game.
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_vegetation <- load_data_vegetation(path_to_fieldmapdb)
#' data_herblayer <- load_data_herblayer(path_to_fieldmapdb)
#' calc_vegetation_plot(data_vegetation, data_herblayer)
#'
#' @export
#'
#' @importFrom dplyr %>% group_by left_join mutate n_distinct relocate select
#' summarise ungroup
#' @importFrom rlang .data
#'
calc_vegetation_plot <- function(data_vegetation, data_herblayer) {
  attributes <-
    compare_attributes(
      data_vegetation, data_herblayer, "data_vegetation", "data_herblayer"
    )
  by_plot <- data_herblayer %>%
    group_by(
      .data$plottype, .data$plot_id, .data$subplot_id, .data$period
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
          "plottype", "plot_id", "subplot_id", "period",
          "year_main_survey", "date_vegetation",
          "moss_cover_min", "moss_cover_max", "moss_cover_mid",
          "herb_cover_min", "herb_cover_max", "herb_cover_mid",
          "shrub_cover_min", "shrub_cover_max", "shrub_cover_mid",
          "tree_cover_min", "tree_cover_max", "tree_cover_mid",
          "waterlayer_cover_min", "waterlayer_cover_max",
          "waterlayer_cover_mid", "soildisturbance_game_cover_min",
          "soildisturbance_game_cover_max",
          "soildisturbance_game_cover_mid"
        ),
      by = c("plottype", "plot_id", "subplot_id", "period")
    ) %>%
    mutate(
      # cumulated_canopy_cover:
      # total cover in percentage = (TL/100 + SL/100 - TL/100 * SL/100) * 100,
      # where TL is the percentage cover of the tree layer
      # and SL is the percentage cover of the shrub layer.
      cumulated_canopy_cover_min =
        100 * (
          1 -
            (1 - .data$shrub_cover_min / 100) * (1 - .data$tree_cover_min / 100)
        ),
      cumulated_canopy_cover_max =
        100 * (
          1 -
            (1 - .data$shrub_cover_max / 100) * (1 - .data$tree_cover_max / 100)
        ),
      cumulated_canopy_cover_mid =
        100 * (
          1 -
            (1 - .data$shrub_cover_mid / 100) * (1 - .data$tree_cover_mid / 100)
        )
    ) %>%
    relocate(
      c("year_main_survey", "date_vegetation"), .before = "number_of_species"
    )

  attr(by_plot, "database") <- attributes[["attr_database"]]
  attr(by_plot, "forrescalc") <- attributes[["attr_forrescalc"]]

  return(by_plot)
}
