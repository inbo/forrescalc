#' Calculate additional variables on stem level
#'
#' This function calculates additional variables based on measurements, such as
#' \itemize{
#'  \item calc_height_m: calculated height based on `dbh_mm` and a species specific diameter-height model
#'  \item basal_area_m2
#'  \item vol_bole_m3: calculated based on `dbh_mm`, `calc_height_m` and species specific tariffs
#'  \item vol_crown_m3: calculated based on `dbh_mm` and species specific tariffs
#'  \item vol_tot_m3: sum of `vol_bole_m3` and `vol_crowwn_m3`
#'  \item basal_area_alive_m2_ha
#'  \item basal_area_dead_m2_ha
#'  \item vol_alive_m3_ha
#'  \item vol_dead_standing_m3_ha
#'  \item vol_bole_alive_m3_ha
#'  \item vol_bole_dead_m3_ha
#' }
#'
#' @inheritParams calculate_dendrometry
#' @param data_stems dataframe on stems (shoots and trees) as given from the function compose_stem_data()
#'
#' @return Dataframe with ...
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_shoots <-
#'   load_data_shoots("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' height_model <- load_height_models("C:/bosreservaten/Hoogtemodellen/")
#' calc_variables_stem_level(data_stems, height_model)
#' }
#'
#' @export
#'
#' @importFrom readr read_csv2
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows filter group_by left_join mutate n select summarise ungroup
#'
calc_variables_stem_level <-
  function(data_stems, height_model) {

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
        select(-.data$model, -.data$P1, -.data$P2) %>%
        left_join(
          height_model %>%
            filter(is.na(.data$species)) %>%
            select(-.data$species),
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
      # if no height_model is available, calc_height_fm on tree level (< FM-IA) is used
      calc_height_m =
        ifelse(is.na(.data$calc_height_r), .data$calc_height_fm, .data$calc_height_r)
    ) %>%
    select(
      -.data$model, -.data$P1, -.data$P2
    ) %>%
  # (2) calculate volume (bole and crown; 1 entry and 2 entries)
    # bole volume 1 entry
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/tariffs1entry.csv", package = "forrescalc")
        )
      ) %>%
        select(-.data$name_nl, -.data$tariff_id, -.data$tariff_group, -.data$source),
      by = "species"
    ) %>%
    mutate(
      perimeter = pi * .data$dbh_mm / 10
      , vol_bole_t1_m3 =
        .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
        .data$d * .data$perimeter ^ 3
      , vol_bole_t1_m3 = pmax(0, .data$vol_bole_t1_m3)
    ) %>%
    select(
      -.data$a, -.data$b, -.data$c, -.data$d
    ) %>%
    # crown volume 1 entry
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/tariffs1entry_crown.csv", package = "forrescalc")
        )
      ) %>%
        select(-.data$name_nl, -.data$tariff_id, -.data$tariff_group, -.data$source),
      by = "species"
    ) %>%
    mutate(
      vol_crown_m3 =
        .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
        .data$d * .data$perimeter ^ 3,
      vol_crown_m3 = pmax(0, .data$vol_crown_m3)
    ) %>%
    select(
      -.data$a, -.data$b, -.data$c, -.data$d
    ) %>%
    # bole volume 2 entries
    # !! (when DH-model or calc_height_fm is available)
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/tariffs2entries.csv", package = "forrescalc")
        )
      ) %>%
        select(-.data$name_nl, -.data$tariff_id, -.data$tariff_group, -.data$source),
      by = "species"
    ) %>%
      mutate(
        d_cm = .data$dbh_mm / 10
        , vol_bole_t2_m3 =
          ifelse(
            .data$formula == 1,
            yes =
              .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
              .data$d * .data$perimeter ^ 3 + .data$e * .data$calc_height_m +
              .data$f * .data$calc_height_m * .data$perimeter +
              .data$g * .data$calc_height_m * .data$perimeter ^ 2,
            no =
              1 / 1000 *
              #spil
              (exp(1.10597 * log(.data$calc_height_m) + 1.78865 * log(.data$d_cm) - 3.07192) -
                 #Verlies
                 exp(-4.608923 * log(.data$d_cm) + 3.005989 * log(.data$calc_height_m) -
                       1.3209 * log(.data$calc_height_m) * log(.data$calc_height_m) +
                       1.605266 * log(.data$d_cm) * log(.data$calc_height_m) + 5.410272))
          )
        , vol_bole_t2_m3 = pmax(0, .data$vol_bole_t2_m3)
        , vol_bole_m3 =
          ifelse(
            .data$ind_sht_cop == 12 & is.na(.data$vol_bole_t2_m3),
            .data$vol_bole_t1_m3,
            .data$vol_bole_t2_m3
          )
      ) %>%
    select(
      -.data$a, -.data$b, -.data$c, -.data$d, -.data$e, -.data$f, -.data$g,
      -.data$formula, -.data$d_cm, -.data$perimeter
    ) %>%
    mutate(
      # volume correction for snags
      vol_crown_m3 = ifelse(.data$intact_snag == 10, 0, .data$vol_crown_m3),
      upper_diam_snag_mm = ifelse(.data$intact_snag == 10,
                                    .data$dbh_mm * (.data$calc_height_m - .data$height_m) / .data$calc_height_m,
                                    NA),
      volume_snag_m3 = ifelse(.data$intact_snag == 10,
                              # as truncated cone - appears to be less accurate!!!!
                              # pi * .data$height_m * (.data$dbh_mm^2 + .data$dbh_mm * .data$upper_diam_snag_mm + .data$upper_diam_snag_mm^2) / (3 * 2000^2),
                              # 1/3 x π x h x ( R² + R x r + r² ) - truncated cone
                              # AS CILINDER - GIVES BETTER RESULTS
                              pi * .data$height_m * .data$dbh_mm^2 / 2000^2,
                              # ! TEMPORARY SOLUTION: goal is to incormporate taper functuions cfr FM-IA
                                NA),
      # !!! ? als calc_height er niet is, dan ev. wel nog als cilinder???
      # nee, want dan ook geen volumes van de andere bomen ...)
      vol_bole_m3 = ifelse(.data$intact_snag == 10,
                             .data$volume_snag_m3,
                             .data$vol_bole_m3)
    ) %>%
    mutate(
      # volume correction for broken crown or branches
      reduction_crown =
        ifelse(is.na(.data$crown_volume_reduction), 0, .data$crown_volume_reduction),
      vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_crown),
      reduction_branch =
        ifelse(is.na(.data$branch_length_reduction), 0, .data$branch_length_reduction),
      vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_branch),
      # total volume
      vol_tot_m3 = .data$vol_bole_m3 + .data$vol_crown_m3
    ) %>%
    select(
      -.data$upper_diam_snag_mm, -.data$volume_snag_m3
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
      -.data$calc_height_fm, -.data$calc_height_r, -.data$dh_model,
      -.data$vol_bole_t1_m3, -.data$vol_bole_t2_m3,
      -.data$reduction_crown, -.data$reduction_branch)

  return(data_stems2)
}
