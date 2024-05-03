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
#' @importFrom dplyr %>% filter group_by left_join mutate rename select ungroup
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
      RegSpecies.IDRegeneration%2$s AS subplot_id,
      RegSpecies.IDHeightClass%2$s AS heightclass_id,
      RegSpecies.ID AS regspecies_id,
      RegSpecies.Species AS species,
      RegSpecies.NumberClass AS number_class,
      RegSpecies.Number AS number_,
      RegSpecies.GameDamage_number AS game_damage_number
    FROM RegSpecies%2$s RegSpecies;"

  data_regspecies <- query_database(database, query_regspecies) %>%
    rename(number = .data$number_)
  data_heightclass <-
    query_database(database, query_heightclass, selection = selection)

  number_classes <-
    data.frame(
      id = c(1, 3, 8, 15, 30, 50, 80, 101, 1001, 0),
      number_class =
        c("1", "2 - 5", "6 - 10", "11 - 20", "21 - 40", "41 - 60", "61 - 100",
          "> 100", "> 1000", "0"),
      approx_nr_regeneration = c(1, 3, 8, 15, 30, 50, 80, 101, 1001, 0),
      min_number_of_regeneration = c(1, 2, 6, 11, 21, 41, 61, 101, 1001, 0),
      max_number_of_regeneration = c(1, 5, 10, 20, 40, 60, 100, 1000, 10000, 0),
      stringsAsFactors = FALSE
    )

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
    left_join(
      number_classes %>%
        select("id", max_number = "max_number_of_regeneration"),
      by = c("number_class" = "id")
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
        ),
      field_game_damage_number =
        ifelse(
          is.na(.data$field_game_damage_number) & .data$game_impact_reg == 10 &
            !is.na(.data$number) & .data$game_damage_number > .data$number,
          "higher than total number", NA
        ),
      field_game_damage_number =
        ifelse(
          is.na(.data$field_game_damage_number) & .data$game_impact_reg == 10 &
            !is.na(.data$number_class) &
            .data$game_damage_number > .data$max_number,
          "higher than total number", NA
        ),
      field_game_damage_number =
        ifelse(
          !is.na(.data$game_damage_number) & .data$game_impact_reg == 20,
          "unexpected (not missing)", NA
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
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      plottype = NULL
    ) %>%
    pivot_longer(
      cols =
        !c("plot_id", "subplot_id", "regspecies_id", "heightclass_id", "period",
           "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(.data$aberrant_field == .data$varname) %>%
    select(-"varname")

  return(incorrect_regspecies)
}
