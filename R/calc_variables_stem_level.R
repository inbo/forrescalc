#' Calculate additional variables on stem level
#'
#' This function calculates additional variables based on measurements, such as
#' \itemize{
#'  \item `calc_height_m`: calculated height based on `dbh_mm` and a species
#'  specific diameter-height model
#'  \item `basal_area_m2`
#'  \item `vol_bole_m3`: calculated based on `dbh_mm`, `calc_height_m` and
#'  species specific tariffs
#'  \item `vol_crown_m3`: calculated based on `dbh_mm` and
#'  species specific tariffs
#'  \item `vol_tot_m3`: sum of `vol_bole_m3` and `vol_crown_m3`
#'  \item `basal_area_alive_m2_ha`
#'  \item `basal_area_dead_m2_ha`
#'  \item `vol_alive_m3_ha`
#'  \item `vol_dead_standing_m3_ha`
#'  \item `vol_bole_alive_m3_ha`
#'  \item `vol_bole_dead_m3_ha`
#' }
#'
#' @inheritParams calculate_dendrometry
#' @param data_stems dataframe on stems (shoots and trees) as given from the
#' function compose_stem_data()
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
#' calc_variables_stem_level(data_stems, height_model)
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows filter left_join mutate select
#'
calc_variables_stem_level <-
  function(data_stems, height_model) {

  attr(height_model, "database") <- attr(data_stems, "database")
  attributes <-
    compare_attributes(
      data_stems, height_model, "data_stems", "height_model"
    )
  # (1) calculate height using height models (calc_height_r)
  data_stems1 <- data_stems %>%
    left_join(
    height_model,
    by = c("species", "forest_reserve", "period", "plottype")
    )

  data_stems2 <- data_stems1 %>%
    filter(!is.na(.data$model)) %>%
    bind_rows(
      data_stems1 %>%
        filter(is.na(.data$model)) %>%
        select(-"model", -"P1", -"P2") %>%
        left_join(
          height_model %>%
            filter(is.na(.data$species)) %>%
            select(-"species"),
          by = c("forest_reserve", "period", "plottype")
        )
    ) %>%
    mutate(
      calc_height_r =
        ifelse(
          grepl("exp", .data$model),
          1.3 + exp(.data$P1 + .data$P2 / (.data$dbh_mm / 10)),
          1.3 + .data$P1 + .data$P2 * log(.data$dbh_mm / 10)
        ),
      dh_model = ifelse(!is.na(.data$P1), TRUE, FALSE),
      # if no height_model is available, calc_height_fm on tree level (< FM-IA)
      # is used
      calc_height_m =
        ifelse(is.na(.data$calc_height_r)
               , pmax(1.3, .data$calc_height_fm)
               , pmax(1.3, .data$calc_height_r))
    ) %>%
    select(
      -"model", -"P1", -"P2"
    )
  data_stems2 <- calc_stem_volume(data_stems2) %>%
    mutate(
      # volume correction for broken crown or branches
      reduction_crown =
        ifelse(is.na(.data$crown_volume_reduction), 0,
               as.numeric(.data$crown_volume_reduction)),
      vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_crown),
      reduction_branch =
        ifelse(is.na(.data$branch_length_reduction), 0,
               as.numeric(.data$branch_length_reduction)),
      vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_branch),
      # total volume
      vol_tot_m3 = .data$vol_bole_m3 + .data$vol_crown_m3
    ) %>%
  # (3) results per hectare
    mutate(stem_number_alive_ha =
             ifelse(
                .data$alive_dead == 11,
                1 / .data$plotarea_ha,
                0
            ),
           stem_number_dead_ha =
             ifelse(
               .data$alive_dead == 12,
               1 / .data$plotarea_ha,
               0
             ),
           basal_area_alive_m2_ha =
             ifelse(
               .data$alive_dead == 11,
               .data$basal_area_m2 / .data$plotarea_ha,
               0
             ),
           basal_area_dead_m2_ha =
             ifelse(
               .data$alive_dead == 12,
               .data$basal_area_m2 / .data$plotarea_ha,
               0
             ),
           vol_alive_m3_ha =
             ifelse(
               .data$alive_dead == 11,
               .data$vol_tot_m3 / .data$plotarea_ha,
               0
             ),
           vol_dead_standing_m3_ha =
             ifelse(
               .data$alive_dead == 12,
               .data$vol_tot_m3 / .data$plotarea_ha,
               0
             ),
           vol_bole_alive_m3_ha =
             ifelse(
               .data$alive_dead == 11,
               .data$vol_bole_m3 / .data$plotarea_ha,
               0
             ),
           vol_bole_dead_m3_ha =
             ifelse(
               .data$alive_dead == 12,
               .data$vol_bole_m3 / .data$plotarea_ha,
               0
             )
    ) %>%
    select(
      -"calc_height_fm", -"calc_height_r", -"dh_model",
      -"reduction_crown", -"reduction_branch")

  attr(data_stems2, "database") <- attributes[["attr_database"]]
  attr(data_stems2, "forrescalc") <- attributes[["attr_forrescalc"]]
  attr(data_stems2, "heightmodels") <- attr(height_model, "heightmodels")

  return(data_stems2)
}
