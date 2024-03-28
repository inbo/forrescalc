#' check table Deadwood from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table Deadwood (of all
#' periods) from the given database and checks for missing data or wrong input.
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
#' check_data_deadwood(path_to_fieldmapdb)
#' check_data_deadwood(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter group_by left_join mutate select summarise
#'   ungroup
#' @importFrom tidyr pivot_longer
#'
check_data_deadwood <- function(database, forest_reserve = "all") {
  selection <-
    ifelse(
      forest_reserve == "all", "",
      paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
    )
  query_deadwood <-
    "SELECT Deadwood.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      Deadwood.ID AS lying_deadw_id,
      Deadwood.IntactFragment AS intact_fragment,
      Deadwood.AliveDead AS alive_dead,
      Deadwood.DecayStage AS decay_stage
    FROM ((Plots
      INNER JOIN Deadwood%2$s Deadwood ON Plots.ID = Deadwood.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_%1$deSet pd ON Plots.ID = pd.IDPlots
    %3$s;"

  query_deadwood_diameters <-
    "SELECT dwd.IDPlots As plot_id,
      dwd.IDDeadwood%2$s AS lying_deadw_id,
      dwd.Distance_m AS distance_m,
      dwd.Diameter_mm AS diameter_mm
    FROM Deadwood%2$s_Diameters dwd;"

  data_deadwood <-
    query_database(database, query_deadwood, selection = selection)
  data_deadwood_diameters <- query_database(database, query_deadwood_diameters)

  incorrect_deadwood <- data_deadwood %>%
    left_join(
      data_deadwood_diameters %>%
        group_by(.data$plot_id, .data$lying_deadw_id) %>%
        summarise(max_diameter_mm = max(.data$diameter_mm)) %>%
        ungroup() %>%
        filter(.data$max_diameter_mm < 100) %>%
        mutate(
          field_max_diameter_mm = "too low"
        ),
      by = c("plot_id", "lying_deadw_id")
    ) %>%
    mutate(
      field_intact_fragment =
        ifelse(is.na(.data$intact_fragment), "missing", NA),
      field_intact_fragment =
        ifelse(
          !is.na(.data$intact_fragment) &
            !.data$intact_fragment %in% c(10, 20, 30), "not in lookuplist",
          .data$field_intact_fragment
        ),
      field_intact_fragment =
        ifelse(
          !is.na(.data$intact_fragment) & .data$intact_fragment == 30 &
            .data$plottype %in% c("CP", "CA"),
          "invalid for plottype",
          .data$field_intact_fragment
        ),
      field_intact_fragment =
        ifelse(
          !is.na(.data$intact_fragment) & .data$intact_fragment == 10 &
            !.data$plottype %in% c("CA", "BE"),
          "invalid for plottype",
          .data$field_intact_fragment
        ),
      field_alive_dead = ifelse(.data$alive_dead == 11, "tree alive", NA),
      field_decay_stage = ifelse(is.na(.data$decay_stage), "missing", NA),
      field_decay_stage =
        ifelse(
          !.data$decay_stage %in% c(10, 11, 12, 13, 14, 15, 16) &
            !is.na(.data$decay_stage),
          "not in lookuplist",
          .data$field_decay_stage),
      field_decay_stage =
        ifelse(
          .data$decay_stage == 16 & .data$alive_dead == 12 &
            !is.na(.data$decay_stage) & !is.na(.data$alive_dead),
          "tree not alive",
          .data$field_decay_stage)
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
        !c("plot_id", "lying_deadw_id", "period", "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(.data$aberrant_field == .data$varname) %>%
    select(-"varname")

  return(incorrect_deadwood)
}
