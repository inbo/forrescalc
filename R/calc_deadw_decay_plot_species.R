#' aggregate parameters by decay stage, plot, tree species and year
#'
#' This function calculates for each plot, tree species and year the volume
#' logs and standing dead wood per hectare and per decay stage.
#'
#' @inheritParams calculate_dendrometry
#' @inheritParams calc_deadw_decay_plot
#'
#' @return dataframe with columns `plot`, `year`, `tree_species`, `decaystage`,
#' `vol_log_m3_ha`
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#'
#' data_deadwood <- load_data_deadwood(path_to_fieldmapdb)
#' data_dendro <- load_data_dendrometry(path_to_fieldmapdb)
#' data_shoots <- load_data_shoots(path_to_fieldmapdb)
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' # omit argument 'example_dataset = TRUE' below to use all height models
#' height_model <- load_height_models(example_dataset = TRUE)
#' data_stems_calc <- calc_variables_stem_level(data_stems, height_model)
#' data_dendro_calc <- calc_variables_tree_level(data_dendro, data_stems_calc)
#' plotinfo <- load_plotinfo(path_to_fieldmapdb)
#' calc_deadw_decay_plot_species(plotinfo, data_deadwood, data_dendro_calc)
#'
#' @export
#'
#' @importFrom dplyr %>% group_by select summarise ungroup
#' @importFrom rlang .data
#'
calc_deadw_decay_plot_species <-
  function(plotinfo, data_deadwood = NA, data_dendro_calc = NA) {

  if (is.data.frame(data_deadwood)) {
    attributes_dw <-
      compare_attributes(
        plotinfo, data_deadwood, "plotinfo", "data_deadwood"
      )
    by_decay_plot_species_log <- data_deadwood %>%
      group_by(
        .data$plottype, .data$plot_id, .data$year, .data$period, .data$species,
        .data$decaystage
      ) %>%
      summarise(
        vol_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
      ) %>%
      ungroup()
  }

  if (is.data.frame(data_dendro_calc)) {
    attributes_dc <-
      compare_attributes(
        plotinfo, data_dendro_calc, "plotinfo", "data_dendro_calc"
      )
    by_decay_plot_species_standing <- data_dendro_calc %>%
      group_by(
        .data$plottype, .data$plot_id, .data$year, .data$period, .data$species,
        .data$decaystage
      ) %>%
      summarise(
        vol_dead_standing_m3_ha = sum(.data$vol_dead_standing_m3_ha),
        vol_bole_dead_m3_ha = sum(.data$vol_bole_dead_m3_ha)
      ) %>%
      ungroup()
  }

  if (is.data.frame(data_deadwood)) {
    if (is.data.frame(data_dendro_calc)) {
      by_decay_plot_species <- by_decay_plot_species_standing %>%
        full_join(
          by_decay_plot_species_log,
          by =
            c("plottype", "plot_id", "year", "period", "species", "decaystage")
        )
    } else {
      by_decay_plot_species <- by_decay_plot_species_log %>%
        mutate(
          vol_dead_standing_m3_ha = NA,
          vol_bole_dead_m3_ha = NA
        )
    }
    attributes <- attributes_dw
  } else {
    if (is.data.frame(data_dendro_calc)) {
      by_decay_plot_species <- by_decay_plot_species_standing %>%
        mutate(
          vol_log_m3_ha = NA
        )
      attributes <- attributes_dc
    } else {
      by_decay_plot_species <- NA
    }
  }

  if (is.data.frame(by_decay_plot_species)) {
    by_decay_plot_species <- plotinfo %>%
      select(
        "plottype", "plot_id", year = "year_dendro", "period", "survey_trees",
        "survey_deadw"
      ) %>%
      left_join(
        by_decay_plot_species, by = c("plottype", "plot_id", "year", "period")
      ) %>%
      filter(.data$survey_trees | .data$survey_deadw) %>%
      mutate(
        vol_log_m3_ha =
          ifelse(
            is.na(.data$vol_log_m3_ha) & .data$survey_trees,
            0,
            .data$vol_log_m3_ha
          ),
        vol_dead_standing_m3_ha =
          ifelse(
            is.na(.data$vol_dead_standing_m3_ha) & .data$survey_deadw,
            0,
            .data$vol_dead_standing_m3_ha
          ),
        vol_bole_dead_m3_ha =
          ifelse(
            is.na(.data$vol_bole_dead_m3_ha) & .data$survey_deadw,
            0,
            .data$vol_bole_dead_m3_ha
          ),
        survey_trees = NULL,
        survey_deadw = NULL
      )
    attr(by_decay_plot_species, "database") <- attributes[["attr_database"]]
    attr(by_decay_plot_species, "forrescalc") <- attributes[["attr_forrescalc"]]
  }
  if (is.data.frame(data_dendro_calc)) {
    attr(by_decay_plot_species, "heightmodels") <-
      attr(data_dendro_calc, "heightmodels")
  }

  return(by_decay_plot_species)
}
