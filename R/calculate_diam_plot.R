#' aggregate parameters by diameter class, plot and year
#'
#' This function calculates for each plot, year and diameter class some values
#' per hectare: number of stems, basal area and volume of standing trees
#' (for coppice based on data on shoot level),
#' and volume of logs (= lying deadwood).
#'
#' @inheritParams calculate_dendrometry
#' @inheritParams calc_variables_tree_level
#'
#' @return dataframe with columns plot, year, dbh_class_5cm, number_of_trees_ha,
#' basal_area_m2_ha, volume_m3_ha
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#'
#' data_dendro <- load_data_dendrometry(path_to_fieldmapdb)
#' data_shoots <- load_data_shoots(path_to_fieldmapdb)
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' height_model <- load_height_models()
#' data_stems_calc <- calc_variables_stem_level(data_stems, height_model)
#' data_deadwood <- load_data_deadwood(path_to_fieldmapdb)
#' plotinfo <- load_plotinfo(path_to_fieldmapdb)
#' calculate_diam_plot(data_stems_calc, data_deadwood, plotinfo)
#'
#' @export
#'
#' @importFrom dplyr %>% across filter full_join group_by mutate select
#'   summarise ungroup
#' @importFrom rlang .data
#'
calculate_diam_plot <- function(data_stems_calc, data_deadwood, plotinfo) {
  attributes1 <-
    compare_attributes(
      data_stems_calc, data_deadwood, "data_stems_calc", "data_deadwood"
    )
  attributes2 <-
    compare_attributes(
      data_stems_calc, plotinfo, "data_stems_calc", "plotinfo"
    )
  by_diam_plot <- data_stems_calc %>%
    group_by(
      .data$plottype, .data$plot_id, .data$year, .data$period,
      .data$dbh_class_5cm
    ) %>%
    summarise(
      stem_number_alive_ha =
        sum(.data$stem_number_alive_ha),
      stem_number_dead_ha =
        sum(.data$stem_number_dead_ha),
      basal_area_alive_m2_ha =
        sum(.data$basal_area_alive_m2_ha),
      basal_area_dead_m2_ha =
        sum(.data$basal_area_dead_m2_ha),
      vol_alive_m3_ha =
        sum(.data$vol_alive_m3_ha),
      vol_dead_standing_m3_ha =
        sum(.data$vol_dead_standing_m3_ha),
      vol_bole_alive_m3_ha =
        sum(.data$vol_bole_alive_m3_ha),
      vol_bole_dead_m3_ha =
        sum(.data$vol_bole_dead_m3_ha)
    ) %>%
    ungroup() %>%
    full_join(
      data_deadwood %>%
        group_by(
          .data$plottype, .data$plot_id, .data$year, .data$period,
          .data$dbh_class_5cm
        ) %>%
        summarise(
          vol_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plottype", "plot_id", "year", "period", "dbh_class_5cm")
    ) %>%
    full_join(
      plotinfo %>%
        select(
          "plottype", "plot_id", year = "year_dendro", "period", "survey_trees",
          "survey_deadw"
        ) %>%
        filter(.data$survey_trees | .data$survey_deadw),
      by = c("plottype", "plot_id", "year", "period")
    ) %>%
    mutate(
      across(
        "stem_number_alive_ha":"vol_bole_dead_m3_ha",
        ~ ifelse(is.na(.x) & survey_trees, 0, .x)
      ),
      vol_log_m3_ha =
        ifelse(
          is.na(.data$vol_log_m3_ha) & .data$survey_deadw,
          0, .data$vol_log_m3_ha
        ),
      survey_trees = NULL,
      survey_deadw = NULL
    )

  attr(by_diam_plot, "database") <- attributes1[["attr_database"]]
  attr(by_diam_plot, "forrescalc") <- attributes1[["attr_forrescalc"]]

  return(by_diam_plot)
}
