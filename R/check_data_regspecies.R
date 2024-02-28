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
#' check_data_regspecies(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% group_by left_join mutate summarise transmute ungroup
#' @importFrom tidyr pivot_longer
#'
check_data_regspecies <- function(database, forest_reserve = "all") {
  selection <-
    ifelse(
      forest_reserve == "all", "",
      paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
    )
  query_heightclass <-
    "SELECT hc.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      pd.GameImpactRegObserved AS game_impact_reg,
      hc.IDRegeneration%2$s AS subplot_id,
      hc.ID AS heightclass_id,
      hc.HeightClass as heightclass
    FROM ((Plots
        INNER JOIN HeightClass%2$s hc ON Plots.ID = hc.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_%1$deSet pd ON Plots.ID = pd.IDPlots
    %3$s;"

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
  data_heightclass <-
    query_database(database, query_heightclass, selection = selection)

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
      data_regspecies %>%
        group_by(.data$plot_id, .data$period) %>%
        mutate(
          not_na_game_damage_number = any(!is.na(.data$game_damage_number))
        ) %>%
        ungroup(),
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
        ),
      field_game_damage_number =
        ifelse(
          is.na(.data$game_damage_number) & .data$game_impact_reg == 10 &
            .data$not_na_game_damage_number,
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
    transmute(
      .data$plot_id, .data$subplot_id, .data$regspecies_id,
      .data$heightclass_id, .data$period,
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      .data$anomaly
    )

  return(incorrect_regspecies)
}
