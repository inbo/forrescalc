#' aggregate parameters by plot and tree species
#'
#' This function calculates for each plot, tree species and year some values
#' per hectare: number of trees, basal area and volume.
#'
#' @inheritParams calc_dendro_plot
#'
#' @return dataframe with columns plot, year, tree_species, number_of_trees_ha,
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
#' data_deadwood <- load_data_deadwood(path_to_fieldmapdb)
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' # omit argument 'example_dataset = TRUE' below to use all height models
#' height_model <- load_height_models(example_dataset = TRUE)
#' data_stems_calc <- calc_variables_stem_level(data_stems, height_model)
#' data_dendro_calc <- calc_variables_tree_level(data_dendro, data_stems_calc)
#' plotinfo <- load_plotinfo(path_to_fieldmapdb)
#' calc_dendro_plot_species(data_dendro_calc, data_deadwood, plotinfo)
#'
#' @export
#'
#' @importFrom dplyr %>% across filter full_join group_by mutate select
#'   summarise ungroup
#' @importFrom rlang .data
#'
calc_dendro_plot_species <-
  function(data_dendro_calc, data_deadwood, plotinfo) {
  attributes <-
    compare_attributes(
      data_dendro_calc, data_deadwood, "data_dendro_calc", "data_deadwood"
    )
  compare_attributes(
    data_dendro_calc, plotinfo, "data_dendro_calc", "plotinfo"
  )
  by_plot_species <- data_dendro_calc %>%
    group_by(
      .data$plottype, .data$plot_id, .data$year, .data$period, .data$species
    ) %>%
    summarise(
      number_of_trees_ha = sum(.data$number_of_trees_alive_ha),
      stem_number_ha =
          sum((.data$alive_dead == 11) * .data$nr_of_stems / .data$plotarea_ha),
      basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
      basal_area_dead_m2_ha = sum(.data$basal_area_dead_m2_ha),
      vol_alive_m3_ha = sum(.data$vol_alive_m3_ha),
      vol_dead_standing_m3_ha = sum(.data$vol_dead_standing_m3_ha),
      vol_bole_alive_m3_ha = sum(.data$vol_bole_alive_m3_ha),
      vol_bole_dead_m3_ha = sum(.data$vol_bole_dead_m3_ha)
    ) %>%
    ungroup() %>%
    full_join(
      data_deadwood %>%
        group_by(
          .data$plottype, .data$plot_id, .data$year, .data$period, .data$species
        ) %>%
        summarise(
          vol_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plottype", "plot_id", "year", "period", "species")
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
        "number_of_trees_ha":"vol_bole_dead_m3_ha",
        ~ ifelse(is.na(.x) & survey_trees, 0, .x)
      ),
      vol_log_m3_ha =
        ifelse(
          is.na(.data$vol_log_m3_ha) & .data$survey_deadw,
          0, .data$vol_log_m3_ha
        ),
      survey_trees = NULL,
      survey_deadw = NULL,
      vol_deadw_m3_ha = .data$vol_dead_standing_m3_ha + .data$vol_log_m3_ha,
      stems_per_tree = .data$stem_number_ha / .data$number_of_trees_ha
    )

  attr(by_plot_species, "database") <- attributes[["attr_database"]]
  attr(by_plot_species, "forrescalc") <- attributes[["attr_forrescalc"]]
  attr(by_plot_species, "heightmodels") <-
    attr(data_dendro_calc, "heightmodels")

  return(by_plot_species)
}
