#' calculate species number by plot, tree height class and year
#'
#' This function calculates for each plot, tree height class and year
#' the number of species, total number of regeneration (or interval with mean
#' and confidence interval) and rubbing damage percentage for regeneration.
#' For core area plots, these variables are calculated for each subplot.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot_id, year, period, height_class,
#' number_of_tree_species, rubbing_damage_perc,
#' nr_of_regeneration_ha (in case exact numbers are observed),
#' mean_number_of_regeneration_ha (in case intervals are observed),
#' lci_number_of_regeneration_ha and uci_number_of_regeneration_ha.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_regeneration_plot_height(data_regeneration)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by n_distinct summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_height <- function(data_regeneration) {
  by_plot_height <- data_regeneration %>%
    mutate(
      plotarea_ha = ifelse(.data$plottype == "CA", 0.01, .data$plotarea_ha)
    ) %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$height_class,
      .data$subplot_id, .data$plotarea_ha
    ) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species, na.rm = TRUE),
      rubbing_damage_perc =
        sum(.data$rubbing_damage_number, na.rm = TRUE) * 100 /
        sum(.data$nr_of_regeneration * (.data$subcircle == "A2"), na.rm = TRUE),
      not_na_rubbing = sum(!is.na(.data$rubbing_damage_perc)),
      nr_of_regeneration_ha =
        sum(.data$nr_of_regeneration, na.rm = TRUE) / unique(.data$plotarea_ha),
      not_na_regeneration = sum(!is.na(.data$nr_of_regeneration)),
      interval =
        sum_intervals(
          var_min = .data$min_number_of_regeneration,
          var_max = .data$max_number_of_regeneration,
          transformation = "log", na_rm = TRUE
        )
    ) %>%
    ungroup() %>%
    mutate(
      nr_of_regeneration_ha =
        ifelse(
          .data$not_na_regeneration > 0 & .data$nr_of_regeneration_ha > 0,
          .data$nr_of_regeneration_ha,
          NA
        ),
      mean_number_of_regeneration_ha = .data$interval$sum / .data$plotarea_ha,
      lci_number_of_regeneration_ha = .data$interval$lci / .data$plotarea_ha,
      uci_number_of_regeneration_ha = .data$interval$uci / .data$plotarea_ha,
      rubbing_damage_perc =
        ifelse(
          .data$not_na_rubbing > 0 & .data$rubbing_damage_perc > 0,
          .data$rubbing_damage_perc,
          NA
        )
    ) %>%
    select(
      -.data$interval, -.data$plotarea_ha, -.data$not_na_regeneration,
      -.data$not_na_rubbing
    )

  return(by_plot_height)
}
