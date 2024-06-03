#' calculate species number by plot and year
#'
#' This function calculates for each plot and year the number of species, total
#' number of seedlings and established regeneration (or interval with mean
#' and confidence interval using a log transformation) and
#' approximate rubbing damage percentage for seedlings and established
#' regeneration.
#' For core area plots, these variables are calculated for each subplot.
#'
#' @inheritParams calculate_regeneration
#'
#' @return dataframe with columns plot, subplot, year, period,
#' number_of_tree_species, nr_of_tree_species_established,
#' mean_number_established_ha, lci_number_established_ha,
#' uci_number_established_ha,
#' mean_number_seedlings_ha, lci_number_seedlings_ha,
#' uci_number_seedlings_ha,
#' mean_rubbing_damage_perc_established, lci_rubbing_damage_perc_established,
#' uci_rubbing_damage_perc_established,
#' mean_rubbing_damage_perc_seedlings, lci_rubbing_damage_perc_seedlings,
#' uci_rubbing_damage_perc_seedlings,
#' approx_nr_established_ha, approx_nr_seedlings_ha,
#' approx_rubbing_damage_perc_established, approx_rubbing_damage_perc_seedlings.
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_regeneration <- load_data_regeneration(path_to_fieldmapdb)
#' calculate_regeneration_plot(data_regeneration)
#'
#' @export
#'
#' @importFrom dplyr %>% distinct filter group_by mutate n_distinct
#'   select summarise ungroup
#' @importFrom rlang .data
#'
calculate_regeneration_plot <- function(data_regeneration) {
  check_forrescalc_version_attr(data_regeneration)
  no_subcircle <- data_regeneration %>%
    filter(
      is.na(.data$subcircle),
      .data$nr_of_regeneration != 0
    ) %>%
    distinct(.data$plot_id)
  if (nrow(no_subcircle) > 0) {
    warning(
      sprintf(
        "Records of dataset data_regeneration with subcircle NA are ignored for this calculation. Such record(s) with nr_of_regeneration different from 0 occur in plot_id %s", #nolint: line_length_linter
        paste(no_subcircle$plot_id, collapse = ", ")
      )
    )
  }
  unique_plotarea_ha <- data_regeneration %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$subplot_id, .data$subcircle
    ) %>%
    summarise(
      n_plotarea_ha = n_distinct(.data$plotarea_ha),
      n_plottype = n_distinct(.data$plottype)
    ) %>%
    ungroup() %>%
    filter(.data$n_plotarea_ha > 1 | .data$n_plottype > 1)
  if (nrow(unique_plotarea_ha) > 0) {
    warning(
      sprintf(
        "Records of dataset data_regeneration from the same plot_id, year, period, subplot_id and subcircle are supposed to have the same plotarea_ha and plottype. This is not the case in plot_id %s. Here the sum of the number of regeneration is divided by the average of the plotarea_ha, without applying any weighing for the area in which the regeneration is observed.", #nolint: line_length_linter
        paste(unique_plotarea_ha$plot_id, collapse = ", ")
      )
    )
  }
  by_plot <- data_regeneration %>%
    mutate(
      species_a2 =
        ifelse(!is.na(.data$subcircle) & .data$subcircle == "A2",
               .data$species, NA),
      plotarea_ha = ifelse(.data$plottype == "CA", 0.01, .data$plotarea_ha),
      min_number_established =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
               .data$min_number_of_regeneration, NA),
      max_number_established =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
               .data$max_number_of_regeneration, NA),
      min_number_seedlings =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$min_number_of_regeneration, NA),
      max_number_seedlings =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$max_number_of_regeneration, NA),
      approx_nr_established =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
               .data$approx_nr_regeneration, NA),
      approx_nr_seedlings =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$approx_nr_regeneration, NA),
      rubbing_damage_established =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
               .data$rubbing_damage_number, NA),
      rubbing_damage_seedlings =
        ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
               .data$rubbing_damage_number, NA)
    ) %>%
    group_by(
      .data$plottype, .data$plot_id, .data$subplot_id, .data$period, .data$year
    ) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species, na.rm = TRUE),
      nr_of_tree_species_established =
        n_distinct(.data$species_a2, na.rm = TRUE),
      plotarea_a1_ha = max(.data$plotarea_ha *
                             (.data$subcircle == "A1" | is.na(subcircle))),
      # "or is.na(subcircle)": to account for plots with no regeneration at all
      plotarea_a2_ha = max(.data$plotarea_ha *
                             (.data$subcircle == "A2"| is.na(subcircle))),
      established_interval =
        sum_intervals(
          var_min = .data$min_number_established,
          var_max = .data$max_number_established,
          transformation = "log", na_rm = TRUE
        ),
      seedlings_interval =
        sum_intervals(
          var_min = .data$min_number_seedlings,
          var_max = .data$max_number_seedlings,
          transformation = "log", na_rm = TRUE
        ),
      rubbing_damage_nr_established =
        sum(.data$rubbing_damage_established, na.rm = TRUE),
      not_na_rubbing_established =
        sum(!is.na(.data$rubbing_damage_established)),
      rubbing_damage_nr_seedlings =
        sum(.data$rubbing_damage_seedlings, na.rm = TRUE),
      not_na_rubbing_seedlings = sum(!is.na(.data$rubbing_damage_seedlings)),
      approx_nr_established =
        sum(.data$approx_nr_established, na.rm = TRUE),
      approx_nr_seedlings = sum(.data$approx_nr_seedlings, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      plotarea_a1_ha = ifelse(plotarea_a1_ha == 0,
                              plotarea_a2_ha,
                              plotarea_a1_ha),
      # to account for plots with established regeneration, but no seedlings
      plotarea_a2_ha = ifelse(plotarea_a2_ha == 0,
                              plotarea_a1_ha,
                              plotarea_a2_ha),
      # to account for plots with seedlings, but no established regeneration
      mean_number_established = .data$established_interval$sum,
      lci_number_established = .data$established_interval$lci,
      uci_number_established = .data$established_interval$uci,
      mean_number_seedlings = .data$seedlings_interval$sum,
      lci_number_seedlings = .data$seedlings_interval$lci,
      uci_number_seedlings = .data$seedlings_interval$uci,
      rubbing_damage_nr_established =
        ifelse(
          .data$not_na_rubbing_established > 0,
          .data$rubbing_damage_nr_established,
          NA
        ),
      rubbing_damage_nr_seedlings =
        ifelse(
          .data$not_na_rubbing_seedlings > 0,
          .data$rubbing_damage_nr_seedlings,
          NA
        ),
      rubbing_damage_nr_established =
        ifelse(
          is.na(.data$rubbing_damage_nr_established) &
            !is.na(.data$rubbing_damage_nr_seedlings),
          0,
          .data$rubbing_damage_nr_established
        ),
      rubbing_damage_nr_seedlings =
        ifelse(
          is.na(.data$rubbing_damage_nr_seedlings) &
            !is.na(.data$rubbing_damage_nr_established),
          0,
          .data$rubbing_damage_nr_seedlings
        ),
      number_of_tree_species =
        ifelse(.data$number_of_tree_species == 0 &
                 is.na(.data$mean_number_seedlings),
               NA,
               .data$number_of_tree_species),
      nr_of_tree_species_established =
        ifelse(.data$nr_of_tree_species_established == 0 &
                 is.na(.data$mean_number_seedlings),
               NA,
               .data$nr_of_tree_species_established),
      # to correct approx_nr_xxx = 0 by sum of NAs (see above)
      approx_nr_established =
        ifelse(.data$approx_nr_established == 0 &
                 is.na(.data$mean_number_established),
               NA,
               .data$approx_nr_established),
      approx_nr_seedlings =
        ifelse(.data$approx_nr_seedlings == 0 &
                 is.na(.data$mean_number_seedlings),
               NA,
               .data$approx_nr_seedlings),
      # per hectare
      approx_nr_established_ha =
        .data$approx_nr_established / .data$plotarea_a2_ha,
      approx_nr_seedlings_ha =
        .data$approx_nr_seedlings / .data$plotarea_a1_ha,
      mean_number_established_ha =
        .data$mean_number_established / .data$plotarea_a2_ha,
      lci_number_established_ha =
        .data$lci_number_established / .data$plotarea_a2_ha,
      uci_number_established_ha =
        .data$uci_number_established / .data$plotarea_a2_ha,
      mean_number_seedlings_ha =
        .data$mean_number_seedlings / .data$plotarea_a1_ha,
      lci_number_seedlings_ha =
        .data$lci_number_seedlings / .data$plotarea_a1_ha,
      uci_number_seedlings_ha =
        .data$uci_number_seedlings / .data$plotarea_a1_ha,
      rubbing_damage_nr_established_ha =
        .data$rubbing_damage_nr_established / .data$plotarea_a2_ha,
      rubbing_damage_nr_seedlings_ha =
        .data$rubbing_damage_nr_seedlings / .data$plotarea_a1_ha,
      # correction for NA due to plotarea_a1_ha or plotarea_a2_ha = 0
      approx_nr_established_ha =
        ifelse(is.na(.data$approx_nr_established_ha) &
                 .data$approx_nr_seedlings_ha > 0
               , 0
               , .data$approx_nr_established_ha),
      approx_nr_seedlings_ha =
        ifelse(is.na(.data$approx_nr_seedlings_ha) &
                 .data$approx_nr_established_ha > 0
               , 0
               , .data$approx_nr_seedlings_ha),
      mean_number_established_ha =
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
      # percentage rubbing
      approx_rubbing_damage_perc_established = pmin(
        .data$rubbing_damage_nr_established * 100 /
          .data$approx_nr_established, 100),
      approx_rubbing_damage_perc_seedlings = pmin(
        .data$rubbing_damage_nr_seedlings * 100 /
          .data$approx_nr_seedlings, 100),
      mean_rubbing_damage_perc_established =
        .data$rubbing_damage_nr_established * 100 /
        .data$mean_number_established,
      lci_rubbing_damage_perc_established =
        .data$rubbing_damage_nr_established * 100 /
        .data$uci_number_established,
      uci_rubbing_damage_perc_established =
        .data$rubbing_damage_nr_established * 100 /
        .data$lci_number_established,
      mean_rubbing_damage_perc_seedlings =
        .data$rubbing_damage_nr_seedlings * 100 /
        .data$mean_number_seedlings,
      lci_rubbing_damage_perc_seedlings =
        .data$rubbing_damage_nr_seedlings * 100 /
        .data$uci_number_seedlings,
      uci_rubbing_damage_perc_seedlings =
        .data$rubbing_damage_nr_seedlings * 100 /
        .data$lci_number_seedlings,
    ) %>%
    select(
      "plottype", "plot_id", "subplot_id", "period", "year",
      "number_of_tree_species", "nr_of_tree_species_established",
      "approx_nr_established_ha", "approx_nr_seedlings_ha",
      "approx_rubbing_damage_perc_established",
      "approx_rubbing_damage_perc_seedlings",
      "rubbing_damage_nr_established_ha", "rubbing_damage_nr_seedlings_ha",
      "mean_number_established_ha",
      "lci_number_established_ha", "uci_number_established_ha",
      "mean_number_seedlings_ha", "lci_number_seedlings_ha",
      "uci_number_seedlings_ha",
      "mean_rubbing_damage_perc_established",
      "lci_rubbing_damage_perc_established",
      "uci_rubbing_damage_perc_established",
      "mean_rubbing_damage_perc_seedlings", "lci_rubbing_damage_perc_seedlings",
      "uci_rubbing_damage_perc_seedlings"
    )

  attr(by_plot, "database") <- attr(data_regeneration, "database")
  attr(by_plot, "forrescalc") <- attr(data_regeneration, "forrescalc")

  return(by_plot)
}
