#' check table RegSpecies from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of tables HeightClass and
#' RegSpecies (of all periods) from the given database and
#' checks for missing data or wrong input.
#'
#' @inheritParams check_data_trees
#'
#' @return Dataframe with inconsistent data with ID's and additional columns
#' `aberrant_field` (which column is wrong) and `anomaly` (what is wrong with
#' the input)
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' check_data_regspecies(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% anti_join bind_rows filter group_by left_join mutate select
#'   summarise ungroup
#' @importFrom tidyr pivot_longer
#'
check_data_regspecies <- function(database) {
  query_heightclass <-
    "SELECT hc.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      hc.IDRegeneration%2$s AS subplot_id,
      hc.ID AS heightclass_id,
      hc.HeightClass as heightclass
    FROM (Plots
        INNER JOIN HeightClass%2$s hc ON Plots.ID = hc.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_regspecies <-
    "SELECT RegSpecies.IDPlots AS plot_id,
      RegSpecies.IDRegeneration%2$s subplot_id,
      RegSpecies.IDHeightClass%2$s AS heightclass_id,
      RegSpecies.ID AS regspecies_id,
      RegSpecies.Species AS species,
      RegSpecies.NumberClass AS number_class,
      RegSpecies.Number AS number,
      RegSpecies.GameDamage_number AS game_damage_number
    FROM RegSpecies%2$s RegSpecies;"

  data_regspecies <- query_database(database, query_regspecies)
  data_heightclass <- query_database(database, query_heightclass)

  incorrect_regspecies <- data_heightclass %>%
    group_by(
      .data$plot_id, .data$subplot_id, .data$heightclass, .data$period
    ) %>%
    mutate(
      n_height_class = n()
    ) %>%
    ungroup() %>%
    mutate(
      field_heightclass =
        ifelse(
          .data$n_height_class > 1,
          paste0(.data$n_height_class, " times the same height class"),
          NA
        ),
      n_height_class = NULL
    ) %>%
    left_join(
      data_regspecies,
      by = c("plot_id", "subplot_id", "heightclass_id", "period")
    ) %>%
    mutate(
      field_number_class =
        ifelse(
          is.na(.data$number_class) &
            .data$heightclass %in% c(1000, 2000, 5000, 6000),
          "missing", NA
        ),
      field_number =
        ifelse(
          is.na(.data$number) &
            .data$heightclass %in% c(3000, 4000, 7000, 8000),
          "missing", NA
        )
    ) %>%
    group_by(
      .data$plot_id, .data$subplot_id, .data$species, .data$heightclass,
      .data$period
    ) %>%
    mutate(
      n_species = n()
    ) %>%
    ungroup() %>%
    mutate(
      field_species =
        ifelse(
          .data$n_species > 1,
          paste0(.data$n_species, " times the same species"),
          NA
        )
    ) %>%
    pivot_longer(
      cols = c(starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field)
    ) %>%
    group_by(
      .data$plot_id, .data$subplot_id, .data$regspecies_id,
      .data$heightclass_id, .data$period
    ) %>%
    summarise(
      aberrant_field = paste0(.data$aberrant_field, collapse = " / "),
      anomaly = paste0(.data$anomaly, collapse = " / ")
    ) %>%
    ungroup()

  return(incorrect_regspecies)
}
