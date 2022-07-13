#' calculate species number by plot and year
#'
#' This function calculates for each plot and year the number of species, total
#' number of seedlings and established regeneration (or interval with mean
#' and confidence interval) and rubbing damage percentage for regeneration
#' (for all height classes together).
#' For core area plots, these variables are calculated for each subplot.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, subplot, year, period,
#' number_of_tree_species, nr_established_ha, nr_seedlings_ha,
#' mean_number_established_ha, lci_number_established_ha,
#' uci_number_established_ha,
#' mean_number_seedlings_ha, lci_number_seedlings_ha,
#' uci_number_seedlings_ha,
#' approx_nr_established_ha, approx_nr_seedlings_ha.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_regeneration_plot(data_regeneration)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot <- function(data_regeneration) {
  by_plot <- data_regeneration %>%
    mutate(
      plotarea_ha = ifelse(.data$plottype == "CA", 0.01, .data$plotarea_ha),
      min_number_established_ha =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
               .data$min_number_of_regeneration / .data$plotarea_ha, NA),
      max_number_established_ha =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
               .data$max_number_of_regeneration / .data$plotarea_ha, NA),
      min_number_seedlings_ha =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$min_number_of_regeneration / .data$plotarea_ha, NA),
      max_number_seedlings_ha =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$max_number_of_regeneration / .data$plotarea_ha, NA),
      approx_nr_established_ha =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
               .data$approx_nr_regeneration / .data$plotarea_ha, NA),
      approx_nr_seedlings_ha =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$approx_nr_regeneration / .data$plotarea_ha, NA)
    ) %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$subplot_id
    ) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species, na.rm = TRUE),
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
      not_na_rubbing = sum(!is.na(.data$rubbing_damage_perc)),
      approx_nr_established_ha =
        mean(.data$approx_nr_established_ha, na.rm = TRUE),
      approx_nr_seedlings_ha = mean(.data$approx_nr_seedlings_ha, na.rm = TRUE)
    ) %>%
    ungroup() %>%
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
        )
    ) %>%
    select(
      -.data$established_interval, -.data$seedlings_interval,
      -.data$not_na_rubbing
    )

  return(by_plot)
}
