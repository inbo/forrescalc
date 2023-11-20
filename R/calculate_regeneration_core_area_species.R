#' aggregate regeneration parameters by plot, species and year
#'
#' This function calculates for each plot, species and year the number of
#' seedlings and established regeneration per ha (or interval with mean
#' and confidence interval using a log transformation), and the number and
#' percentage of subplots in which the species is regenerating.
#' This calculation is designed for core areas,that consist of different
#' subplots.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, species, year,
#' nr_of_subplots_with_regeneration, perc_subplots_with_regeneration,
#' mean_number_established_ha,
#' lci_number_established_ha, uci_number_established_ha,
#' mean_number_seedlings_ha,
#' lci_number_seedlings_ha, uci_number_seedlings_ha,
#' approx_nr_established_ha, approx_nr_seedlings_ha.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_regeneration_CA <-
#'   load_data_regeneration(
#'     "C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb",
#'     plottype = "CA"
#'   )
#' plotinfo <-
#'   load_plotinfo("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_regeneration_core_area_species(data_regeneration_CA, plotinfo)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct right_join select summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_core_area_species <- function(data_regeneration, plotinfo) {
  no_subcircle <- data_regeneration %>%
    filter(
      is.na(.data$subcircle),
      .data$nr_of_regeneration != 0
    ) %>%
    distinct(.data$plot_id)
  if (nrow(no_subcircle) > 0) {
    warning(
      sprintf(
        "Records of dataset data_regeneration with subcircle NA are ignored for this calculation. Such record(s) with nr_of_regeneration different from 0 occur in plot_id %s",
        paste(no_subcircle$plot_id, collapse = ", ")
      )
    )
  }
  by_plot_species <- data_regeneration %>%
    group_by(.data$plot_id, .data$period) %>%
    mutate(
      n_subplots = n_distinct(.data$subplot_id),
      min_number_established_ha =
        ifelse(!is.na(.data$subcircle) & .data$subcircle == "A2",
               .data$min_number_of_regeneration / .data$plotarea_ha, NA),
      max_number_established_ha =
        ifelse(!is.na(.data$subcircle) & .data$subcircle == "A2",
               .data$max_number_of_regeneration / .data$plotarea_ha, NA),
      min_number_seedlings_ha =
        ifelse(!is.na(.data$subcircle) & .data$subcircle == "A1",
               .data$min_number_of_regeneration / .data$plotarea_ha, NA),
      max_number_seedlings_ha =
        ifelse(!is.na(.data$subcircle) & .data$subcircle == "A1",
               .data$max_number_of_regeneration / .data$plotarea_ha, NA),
      approx_nr_established_ha =
        ifelse(!is.na(.data$subcircle) & .data$subcircle == "A2",
               .data$approx_nr_regeneration / .data$plotarea_ha, NA),
      approx_nr_seedlings_ha =
        ifelse(!is.na(.data$subcircle) & .data$subcircle == "A1",
               .data$approx_nr_regeneration / .data$plotarea_ha, NA)
    ) %>%
    ungroup() %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$species
    ) %>%
    summarise(
      nr_of_subplots_with_regeneration = n_distinct(.data$subplot_id),
      perc_subplots_with_regeneration =
        .data$nr_of_subplots_with_regeneration * 100 / unique(.data$n_subplots),
      established_interval =
        sum_intervals(
          var_min = .data$min_number_established_ha,
          var_max = .data$max_number_established_ha,
          transformation = "log", na_rm = TRUE
        ),
      seedlings_interval =
        sum_intervals(
          var_min = .data$min_number_seedlings_ha,
          var_max = .data$max_number_seedlings_ha,
          transformation = "log", na_rm = TRUE
        ),
      rubbing_damage_perc =
        sum(.data$rubbing_damage_number, na.rm = TRUE) * 100 /
        sum(.data$nr_of_regeneration * (.data$subcircle == "A2"), na.rm = TRUE),
      not_na_rubbing = sum(!is.na(.data$rubbing_damage_number)),
      approx_nr_established_ha =
        sum(.data$approx_nr_established_ha, na.rm = TRUE),
      approx_nr_seedlings_ha = sum(.data$approx_nr_seedlings_ha, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    right_join(
      plotinfo %>%
        select(
          "plot_id", "period", "game_impact_reg"
        ),
      by = c("plot_id", "period")
    ) %>%
    mutate(
      mean_number_established_ha = .data$established_interval$sum,
      lci_number_established_ha = .data$established_interval$lci,
      uci_number_established_ha = .data$established_interval$uci,
      mean_number_seedlings_ha = .data$seedlings_interval$sum,
      lci_number_seedlings_ha = .data$seedlings_interval$lci,
      uci_number_seedlings_ha = .data$seedlings_interval$uci,
      rubbing_damage_perc =
        ifelse(
          .data$not_na_rubbing > 0 & .data$rubbing_damage_perc > 0,
          .data$rubbing_damage_perc,
          NA
        ),
      rubbing_damage_perc =
        ifelse(
          is.na(.data$rubbing_damage_perc) & .data$game_impact_reg,
          0,
          .data$rubbing_damage_perc
        )
    ) %>%
    mutate(mean_number_established_ha =
             ifelse(is.na(.data$mean_number_established_ha)
                    & .data$mean_number_seedlings_ha > 0
                    , 0
                    , .data$mean_number_established_ha),
           lci_number_established_ha =
             ifelse(is.na(.data$lci_number_established_ha)
                    & .data$mean_number_seedlings_ha > 0
                    , 0
                    , .data$lci_number_established_ha),
           uci_number_established_ha =
             ifelse(is.na(.data$uci_number_established_ha)
                    & .data$mean_number_seedlings_ha > 0
                    , 0
                    , .data$uci_number_established_ha),
           mean_number_seedlings_ha =
             ifelse(is.na(.data$mean_number_seedlings_ha)
                    & .data$mean_number_established_ha > 0
                    , 0
                    , .data$mean_number_seedlings_ha),
           lci_number_seedlings_ha =
             ifelse(is.na(.data$lci_number_seedlings_ha)
                    & .data$mean_number_established_ha > 0
                    , 0
                    , .data$lci_number_seedlings_ha),
           uci_number_seedlings_ha =
             ifelse(is.na(.data$uci_number_seedlings_ha)
                    & .data$mean_number_established_ha > 0
                    , 0
                    , .data$uci_number_seedlings_ha),
           approx_nr_established_ha =
             ifelse(is.na(.data$approx_nr_established_ha)
                    & .data$approx_nr_seedlings_ha > 0
                    , 0
                    , .data$approx_nr_established_ha),
           approx_nr_seedlings_ha =
             ifelse(is.na(.data$approx_nr_seedlings_ha)
                    & .data$approx_nr_established_ha > 0
                    , 0
                    , .data$approx_nr_seedlings_ha)
    ) %>%
    select(
      -.data$established_interval, -.data$seedlings_interval,
      -.data$not_na_rubbing, -.data$game_impact_reg
    )

  return(by_plot_species)
}
