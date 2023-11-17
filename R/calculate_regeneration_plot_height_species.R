#' aggregate parameters by plot, tree height class, species and year
#'
#' This function calculates for each plot, tree height class, species and year
#' the number of regeneration (or interval with mean and confidence interval
#' using a log transformation) and
#' rubbing damage percentage per hectare for regeneration.
#' For core area plots, these variables are calculated for each subplot.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, subplot, year, height_class, species,
#' rubbing_damage_perc, mean_number_of_regeneration_ha,
#' lci_number_of_regeneration_ha, uci_number_of_regeneration_ha and
#' approx_nr_regeneration_ha.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_regeneration <-
#'   load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' plotinfo <-
#'   load_plotinfo("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' calculate_regeneration_plot_height_species(data_regeneration, plotinfo)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by right_join select summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot_height_species <-
  function(data_regeneration, plotinfo) {
  by_plot_height_species <- data_regeneration %>%
    mutate(
      plotarea_ha = ifelse(.data$plottype == "CA", 0.01, .data$plotarea_ha)
    ) %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$height_class,
      .data$species, .data$subplot_id, .data$plotarea_ha
    ) %>%
    summarise(
      rubbing_damage_perc =
        sum(.data$rubbing_damage_number, na.rm = TRUE) * 100 /
        sum(.data$nr_of_regeneration * (.data$subcircle == "A2"), na.rm = TRUE),
      not_na_rubbing = sum(!is.na(.data$rubbing_damage_number)),
      interval =
        sum_intervals(
          var_min = .data$min_number_of_regeneration,
          var_max = .data$max_number_of_regeneration,
          transformation = "log", na_rm = TRUE
        ),
      approx_nr_regeneration_ha =
        sum(.data$approx_nr_regeneration) / unique(.data$plotarea_ha)
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
      mean_number_of_regeneration_ha = .data$interval$sum / .data$plotarea_ha,
      lci_number_of_regeneration_ha = .data$interval$lci / .data$plotarea_ha,
      uci_number_of_regeneration_ha = .data$interval$uci / .data$plotarea_ha,
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
    select(
      -.data$interval, -.data$plotarea_ha,
      -.data$not_na_rubbing, -.data$game_impact_reg
    )

  return(by_plot_height_species)
}
