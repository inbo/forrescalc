#' Calculate stem volume including bole and crown volume
#'
#' This function calculates the bole and crown volume to be used
#' in functions `calc_variables_stem_level()` and `calc_intact_deadwood().
#' The volume of snags is calculated as a cilinder. This is a temporary solution,
#' as the goal is to incorporate taper functions cfr FM-IA.
#' Volume calculation as a truncated cone (1/3 x π x h x ( R² + R x r + r² ))
#' appears to be less accurate (upper_diam_snag is needed)
#' (upper_diam_snag_mm = dbh_mm * (calc_height_m - height_m) / calc_height_m)
#' (volume truncated cone = pi x height_m x
#' (dbh_mm^2 + dbh_mm x upper_diam_snag_mm + upper_diam_snag_mm^2)/(3 x 2000^2))
#'
#' @param data_stems dataframe on stems (shoots and trees) as given from the
#' first part of the function `compose_stem_data()`,
#' with variables plot_id, tree_measure_id, period, species,
#' dbh_mm, height_m, intact_snag, calc_height_m
#'
#' @return Dataframe of stem data with vol_bole_m3 and vol_crown_m3 as extra variables
#'
#' @noRd
#'
#' @importFrom readr read_csv2
#' @importFrom rlang .data
#' @importFrom dplyr %>% left_join mutate select
#'
calc_stem_volume <- function(data_stems) {

  check_forrescalc_version_attr(data_stems)
  data_stems <- data_stems %>%
    # (1) calculate bole volume - tariff 1 entry
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/tariffs1entry.csv", package = "forrescalc")
        )
      ) %>%
        select(
          -"name_nl", -"tariff_id", -"tariff_group", -"source"
        ),
      by = "species"
    ) %>%
    mutate(
      perimeter = pi * .data$dbh_mm / 10,
      vol_bole_t1_m3 =
        .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
        .data$d * .data$perimeter ^ 3,
      vol_bole_t1_m3 = pmax(0, .data$vol_bole_t1_m3)
    ) %>%
    select(
      -"a", -"b", -"c", -"d"
    ) %>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/convert_perimeter.csv", package = "forrescalc")
        )
      ),
      by = "species"
    ) %>%
    mutate(
      perimeter_150 = (.data$perimeter - .data$a) / .data$b
    ) %>%
    select(
      -"a", -"b"
    ) %>%
    # (2) calculate crown volume - tariff 1 entry
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/tariffs1entry_crown.csv", package = "forrescalc")
        )
      ) %>%
        select(
          -"name_nl", -"tariff_id", -"tariff_group", -"source"
        ),
      by = "species"
    ) %>%
    mutate(
      vol_crown_m3 =
        .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
        .data$d * .data$perimeter ^ 3,
      vol_crown_m3 = pmax(0, .data$vol_crown_m3)
    ) %>%
    select(
      -"a", -"b", -"c", -"d"
    ) %>%
    # (3) calculate bole volume - tariff 2 entries
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/tariffs2entries.csv", package = "forrescalc")
        )
      ) %>%
        select(
          -"name_nl", -"tariff_id", -"tariff_group", -"source"
        ),
      by = "species"
    ) %>%
    mutate(
      perimeter =
        ifelse(.data$formula == 3, .data$perimeter_150, .data$perimeter),
      d_cm = .data$dbh_mm / 10,
      vol_bole_t2_m3 =
        ifelse(
          .data$formula %in% c(1, 3),
          yes =
            .data$a + .data$b * .data$perimeter +
            .data$c * .data$perimeter ^ 2 +
            .data$d * .data$perimeter ^ 3 +
            .data$e * .data$calc_height_m +
            .data$f * .data$calc_height_m * .data$perimeter +
            .data$g * .data$calc_height_m * .data$perimeter ^ 2,
          no =
            1 / 1000 *
            #spil
            (exp(1.10597 * log(.data$calc_height_m) +
                   1.78865 * log(.data$d_cm) - 3.07192) -
               #Verlies
               exp(
                 -4.608923 * log(.data$d_cm) +
                 3.005989 * log(.data$calc_height_m) -
                 1.3209 * log(.data$calc_height_m) * log(.data$calc_height_m) +
                 1.605266 * log(.data$d_cm) * log(.data$calc_height_m) +
                 5.410272
               )
             )
        ),
      vol_bole_t2_m3 = pmax(0, .data$vol_bole_t2_m3),
      vol_bole_m3 =
        ifelse(
          .data$ind_sht_cop == 12 & is.na(.data$vol_bole_t2_m3),
          .data$vol_bole_t1_m3,
          .data$vol_bole_t2_m3
        )
    ) %>%
    select(
      -"a", -"b", -"c", -"d", -"e", -"f", -"g",
      -"formula", -"d_cm", -"perimeter", -"perimeter_150"
    ) %>%
    mutate(
      # volume correction for snags
      vol_crown_m3 = ifelse(.data$intact_snag == 10, 0, .data$vol_crown_m3),
      upper_diam_snag_mm =
        ifelse(
          .data$intact_snag == 10,
          .data$dbh_mm * (.data$calc_height_m - .data$height_m) /
            .data$calc_height_m,
          NA
        ),
      volume_snag_m3 =
        ifelse(
          .data$intact_snag == 10,
          # as truncated cone - appears to be less accurate!!!!
          # pi * .data$height_m * (.data$dbh_mm^2 + .data$dbh_mm * .data$upper_diam_snag_mm + .data$upper_diam_snag_mm^2) / (3 * 2000^2),
          # 1/3 x π x h x ( R² + R x r + r² ) - truncated cone
          # AS CILINDER - GIVES BETTER RESULTS
          pi * .data$height_m * .data$dbh_mm^2 / 2000^2,
          # ! TEMPORARY SOLUTION: goal is to incorporate taper functions cfr FM-IA
            NA),
      # !!! ? als calc_height er niet is, dan ev. wel nog als cilinder???
      # nee, want dan ook geen volumes van de andere bomen ...)
      vol_bole_m3 =
        ifelse(
          .data$intact_snag == 10,
          .data$volume_snag_m3,
          .data$vol_bole_m3
        )
    ) %>%
    select(
      -"upper_diam_snag_mm", -"volume_snag_m3",
      -"vol_bole_t1_m3", -"vol_bole_t2_m3"
    )

  return(data_stems)
}
