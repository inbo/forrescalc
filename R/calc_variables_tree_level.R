#' Calculate additional variables on tree level
#'
#' This function calculates additional variables based on measurements, such as
#' \itemize{
#'  \item `nr_of_stems`: the number of shoots in the tree
#'  (= 1 for an individual tree; >= 1 when coppice)
#'  \item `individual`: true for individual tree or coppice,
#'  false if record is a secondary shoot
#'  \item `calc_height_m`: calculated height based on `dbh_mm` and
#'  a species specific diameter-height model
#'  \item `basal_area_m2`
#'  \item `vol_bole_m3`: calculated based on `dbh_mm`, `calc_height_m` and
#'  species specific tariffs
#'  \item `vol_crown_m3`: calculated based on `dbh_mm` and
#'  species specific tariffs
#'  \item `vol_tot_m3`: sum of `vol_bole_m3` and `vol_crowwn_m3`
#'  \item `dbh_mm` (based on average for coppice trees)
#'  \item `decaystage` (based on average for coppice trees)
#'  \item `basal_area_alive_m2_ha`
#'  \item `basal_area_dead_m2_ha`
#'  \item `vol_alive_m3_ha`
#'  \item `vol_dead_standing_m3_ha`
#'  \item `vol_bole_alive_m3_ha`
#'  \item `vol_bole_dead_m3_ha`
#' }
#'
#' @inheritParams calculate_dendrometry
#' @param data_stems_calc dataframe on stem level measurements with variables
#' `plot_id`, `plottype`, `tree_measure_id`, `date_dendro`, `dbh_mm`,
#' `height_m`, `species`, `alive_dead`, `decaystage`, `period`, `year`,
#' `subcircle`, `plotarea_ha`,...
#' (output of function `calc_variables_stem_level()`)
#'
#' @return Dataframe with ...
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
#' # omit argument 'example_dataset = TRUE' below to use all height models
#' height_model <- load_height_models(example_dataset = TRUE)
#' data_stems_calc <- calc_variables_stem_level(data_stems, height_model)
#' calc_variables_tree_level(data_dendro, data_stems_calc)
#'
#' @export
#'
#' @importFrom readr read_csv2
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows filter group_by left_join mutate n select
#' summarise ungroup
#'
calc_variables_tree_level <-
  function(data_dendro, data_stems_calc) {

  attributes <-
    compare_attributes(
      data_dendro, data_stems_calc, "data_dendro", "data_stems_calc"
    )
  data_dendro1 <- data_dendro %>%
    select(
      -"dbh_mm", -"nr_of_stems", -"calc_height_fm",
      -"intact_snag", -"decaystage"
    ) %>%
    left_join(
      data_stems_calc %>%
        group_by(.data$plot_id, .data$tree_measure_id, .data$period) %>%
        summarise(
          nr_of_stems = n(),
          decaystage =
            as.integer(
              round(
                sum(.data$decaystage * .data$dbh_mm ^ 2 / 4) /
                  sum(.data$dbh_mm ^ 2 / 4)
              )
            ),
          intact_snag = max(.data$intact_snag),
          calc_height_m = sum(.data$calc_height_m * .data$dbh_mm ^ 2 / 4) /
            sum(.data$dbh_mm ^ 2 / 4),
          dbh_mm = round(sqrt(sum(.data$dbh_mm ^ 2) / n())),
          basal_area_m2 = sum(.data$basal_area_m2),
          vol_bole_m3 = sum(.data$vol_bole_m3),
          vol_crown_m3 = sum(.data$vol_crown_m3),
          vol_tot_m3 = sum(.data$vol_tot_m3),
          # RESULTS PER HECTARE
          basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
          basal_area_dead_m2_ha = sum(.data$basal_area_dead_m2_ha),
          vol_alive_m3_ha = sum(.data$vol_alive_m3_ha),
          vol_dead_standing_m3_ha = sum(.data$vol_dead_standing_m3_ha),
          vol_bole_alive_m3_ha = sum(.data$vol_bole_alive_m3_ha),
          vol_bole_dead_m3_ha = sum(.data$vol_bole_dead_m3_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "tree_measure_id", "period")
    ) %>%
    mutate(
      individual = (.data$ind_sht_cop == 10 | .data$ind_sht_cop == 12)
    ) %>%
    mutate(number_of_trees_alive_ha =
             ifelse(
               .data$alive_dead == 11,
               .data$individual / .data$plotarea_ha,
               0
             ),
           number_of_trees_dead_ha =
             ifelse(
               .data$alive_dead == 12,
               .data$individual / .data$plotarea_ha,
               0
             )
    ) %>%
    select(-"individual")

  attr(data_dendro1, "database") <- attributes[["attr_database"]]
  attr(data_dendro1, "forrescalc") <- attributes[["attr_forrescalc"]]
  attr(data_dendro1, "heightmodels") <- attr(data_stems_calc, "heightmodels")

  return(data_dendro1)
}
