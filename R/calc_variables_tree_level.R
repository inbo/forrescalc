#' Calculate additional variables on tree level
#'
#' This function calculates additional variables based on measurements, such as
#' \itemize{
#'  \item tree_number: the number of shoots in the tree (> 1 when coppice)
#'  \item individual: true if secondary shoot is present, false for individual tree and coppice shoot
#'  \item calc_height_m: calculated height based on `dbh_mm` and a model
#'  \item basal_area_m2
#'  \item vol_tot_m3
#'  \item vol_stem_m3
#'  \item vol_crown_m3
#'  \item dbh_mm for coppice trees
#' }
#'
#' @inheritParams calculate_dendrometry
#' @param data_shoots dataframe on shoots as given from the function load_data_shoots()
#' @param height_model dataframe with `model` containing 'exp' or 'ln',
#' coeficients `P1` and `P2` to calculate height model for each combination of
#' `species`, `forest_reserve`, `period` and `plot_type`
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
#' calc_variables_tree_level(data_dendro, data_shoots, data_stems)
#' }
#'
#' @export
#'
#' @importFrom readr read_csv2
#' @importFrom rlang .data
#' @importFrom dplyr %>% group_by left_join mutate n summarise ungroup
#'
calc_variables_tree_level <-
  function(data_dendro, data_shoots, data_stems, height_model) {
  test <- data_dendro %>%
    left_join(
      data_stems %>%
        group_by(.data$plot_id, .data$tree_measure_id, .data$period) %>%
        summarise(tree_number = n()) %>%
        ungroup(),
      by = c("plot_id", "tree_measure_id", "period")
    ) %>%
    mutate(
      individual = (.data$ind_sht_cop == 11)
    ) %>%
    left_join(
      height_model,
      by = c("species", "forest_reserve", "period", "plottype")
    ) %>%
    mutate(
      calc_height_r =
        ifelse(
          grepl("exp", .data$model),
          1.3 + exp(.data$P1 + .data$P2 / (.data$dbh_mm / 10)),
          1.3 + .data$P1 + .data$P2 * log(.data$dbh_mm / 10)
        ),
      calc_height_m =
        ifelse(is.na(.data$calc_height_r), .data$calc_height_m, .data$calc_height_r)
    ) %>%
    left_join(
      data_stems %>%
        left_join(
          suppressMessages(
            read_csv2(
              system.file("extdata/inst/extdata/tarieven1ing.csv", package = "forrescalc")
            )
          ) %>%
            select(-.data$name_nl, -.data$tarief, -.data$groepnaam, -.data$tarief_id),
          by = "species"
        ) %>%
        mutate(
          perimeter = pi * .data$dbh_mm / 10,
          vol_stem_t1_m3 =
            .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
            .data$d * .data$perimeter ^ 3
        ) %>%
        group_by(.data$plot_id, .data$tree_measure_id, .data$period) %>%
        summarise(
          basal_area_m2 = sum(.data$basal_area_m2),
          vol_stem_t1_m3 = sum(.data$vol_stem_t1_m3),
          perimeter = mean(.data$perimeter)
        ) %>%
        ungroup(),
      by = c("plot_id", "tree_measure_id", "period")
    )  %>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/inst/extdata/tarieven2ing.csv", package = "forrescalc")
        )
      ) %>%
        select(-.data$name_nl, -.data$tarief, -.data$groepnaam, -.data$tarief_id),
      by = "species"
    ) %>%
    mutate(
      perimeter = pi * .data$dbh_mm / 10,
      radius_m = .data$dbh_mm / 2000,
      basal_area_m2 = pi * .data$radius_m ^ 2,
      d_cm = .data$dbh_mm / 10,
      vol_stem_m3 =
        ifelse(
          .data$formule_type == 1,
          .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
            .data$d * .data$perimeter ^ 3 + .data$e * .data$calc_height_m +
            .data$f * .data$calc_height_m * .data$perimeter +
            .data$g * .data$calc_height_m * .data$perimeter ^ 2,
          1 / 1000 *
            #spil
            (exp(1.10597 * log(.data$calc_height_m) + 1.78865 * log(.data$d_cm) - 3.07192) -
               #Verlies
               exp(-4.608923 * log(.data$d_cm) + 3.005989 * log(.data$calc_height_m) -
                     1.3209 * log(.data$calc_height_m) * log(.data$calc_height_m) +
                     1.605266 * log(.data$d_cm) * log(.data$calc_height_m) + 5.410272))
        ) %>%
        select(
          -.data$a, -.data$b, -.data$c, -.data$d, -.data$e, -.data$f, -.data$g,
          -.data$formule_type, -.data$d_cm
        )
    ) %>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/inst/extdata/tarieven1ing_crown.csv", package = "forrescalc")
        )
      ) %>%
        select(-.data$name_nl, -.data$tarief, -.data$groepnaam, -.data$tarief_id),
      by = "species"
    ) %>%
    mutate(
      vol_crown_m3 =
        .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
        .data$d * .data$perimeter ^ 3,
      vol_crown_m3 = pmax(0, .data$vol_crown_m3),
      reduction_crown =
        ifelse(is.na(.data$crown_volume_reduction), 0, .data$crown_volume_reduction),
      vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_crown),
      reduction_branch =
        ifelse(is.na(.data$branch_length_reduction), 0, .data$branch_length_reduction),
      vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_branch),
      vol_tot_m3 = .data$vol_stem_m3 + .data$vol_crown_m3
    ) %>%
    select(
      -.data$a, -.data$b, -.data$c, -.data$d, -.data$perimeter, -.data$radius_m
    )

  return(data_dendro)
}
