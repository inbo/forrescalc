#' @title calculate DAS indicator
#'
#' @description
#' This function does all preparing steps (selection of relevant plots and
#' reserves) and calculations to obtain the DAS-indicator.
#'
#' First the relevant forest reserves are selected based on the following
#' criteria:
#'
#' @template selection_criteria_for_DAS
#'
#' @template calculate_das_indicator_explanation_part2
#'
#' @inheritParams calculate_dendro_plot
#' @param na_rm Ignore records with no value? Default is FALSE, so no records
#' will be ignored unless it is explicitly mentioned by `na.rm = TRUE`
#'
#' @return dataframe with results for DAS indicator on forest level (?)
#'
#' @examples
#' \dontrun{
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
#' data_dendro_calc <- calc_variables_tree_level(data_dendro, data_stems_calc)
#' calculate_das_indicator(data_dendro_calc)
#' }
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% distinct filter group_by inner_join left_join mutate n
#' select summarise ungroup
#' @importFrom readr read_delim
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#'
calculate_das_indicator <- function(data_dendro_calc, na_rm = FALSE) {
  #only consider living trees
  data_dendro_calc <- data_dendro_calc %>%
    filter(.data$alive_dead == 11)

  if (na_rm) {
    data_dendro_calc <- data_dendro_calc %>%
      filter(
        !is.na(.data$dbh_mm),
        !is.na(.data$basal_area_alive_m2_ha)
      )
  }

  assert_that(
    all(!is.na(data_dendro_calc$dbh_mm)),
    msg = "Not all records have a value for 'dbh_mm'"
  )
  assert_that(
    all(!is.na(data_dendro_calc$basal_area_alive_m2_ha)),
    msg = "Not all records have a value for 'basal_area_alive_m2_ha'"
  )

  #select relevant plots
  das_reserve <- data_dendro_calc %>%
    select_for_das_indicator(grouping_vars = c("forest_reserve", "year",
                                               "period")) %>%
    inner_join(data_dendro_calc, by = c("forest_reserve", "year"
                                        , "period")) %>%
    select_for_das_indicator(grouping_vars = c("plot_id", "year"
                                               , "period")) %>%
    inner_join(
      data_dendro_calc %>%
        select("plot_id", "forest_reserve") %>%
        distinct(),
      by = c("plot_id")
    ) %>%
    group_by(.data$forest_reserve, .data$period, .data$year) %>%
    mutate(
      n_plots = n()
    ) %>%
    ungroup() %>%
    filter(.data$n_plots >= 12) %>%
    #add data again to selected plots and calculate difference in basal area
    # proportion
    inner_join(data_dendro_calc, by = c("forest_reserve", "plot_id", "year"
                                        , "period")) %>%
    left_join(
      read_delim(
        system.file("extdata/DAS_tree_groups.csv", package = "forrescalc"),
        delim = ";", col_types = "cd"
      ),
      by = "species"
    ) %>%
    group_by(.data$forest_reserve, .data$period, .data$year, .data$group,
             .data$plot_id) %>%
    summarise(
      basal_area_m2_ha = sum(.data$basal_area_alive_m2_ha)
    ) %>%
    ungroup() %>%
    group_by(.data$forest_reserve, .data$period, .data$year, .data$group) %>%
    summarise(
      basal_area_m2_ha = mean(.data$basal_area_m2_ha)
    ) %>%
    ungroup() %>%
    group_by(.data$forest_reserve, .data$period, .data$year) %>%
    mutate(
      basal_area_proportion =
        .data$basal_area_m2_ha / sum(.data$basal_area_m2_ha)
    ) %>%
    ungroup() %>%
    pivot_wider(
      names_from = .data$period,
      values_from = c("year", "basal_area_m2_ha", "basal_area_proportion")
    ) %>%
    group_by(.data$forest_reserve) %>%
    mutate(
      d_res =
        (sum(.data$basal_area_m2_ha_2) / sum(.data$basal_area_m2_ha_1)) ^
           (1 / (.data$year_2 - .data$year_1)) - 1,
      d_group =
        (.data$basal_area_proportion_2 / .data$basal_area_proportion_1) ^
           (1 / (.data$year_2 - .data$year_1)) - 1
    ) %>%
    ungroup()

  return(das_reserve)
}
