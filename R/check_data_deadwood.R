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
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter group_by left_join mutate summarise transmute
#'   ungroup
#' @importFrom tidyr pivot_longer
#'
check_data_deadwood <- function(database) {
  query_deadwood <-
    "SELECT Deadwood.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      Deadwood.ID AS lying_deadw_id,
      --Deadwood.Xbase_m, Deadwood.Ybase_m,
      --Deadwood.DBHClass_5cm AS dbhclass_5cm,
      --Deadwood.Length_m AS length_m,
      --Deadwood.Species AS species,
      Deadwood.IntactFragment AS intact_fragment,
      Deadwood.AliveDead AS alive_dead,
      Deadwood.DecayStage AS decay_stage
    FROM (Plots INNER JOIN Deadwood%2$s Deadwood ON Plots.ID = Deadwood.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_deadwood_diameters <-
    "SELECT IDPlots As plot_id,
      IDDeadwood%2$s AS lying_deadw_id,
      Distance_m AS distance_m,
      Diameter_mm AS diameter_mm
    FROM Deadwood%2$s_Diameters"

  data_deadwood <- query_database(database, query_deadwood)
  data_deadwood_diameters <- query_database(database, query_deadwood_diameters)

  incorrect_deadwood <- data_deadwood %>%
    left_join(
      data_deadwood_diameters %>%
        group_by(.data$plot_id, .data$lying_deadw_id) %>%
        summarise(max_diameter_mm = max(.data$diameter_mm)) %>%
        ungroup() %>%
        filter(.data$max_diameter_mm < 100) %>%
        transmute(
          .data$plot_id, .data$lying_deadw_id,
          field_deadwood_diameter_mm = "too low"
        ),
      by = c("plot_id", "lying_deadw_id")
    ) %>%
    mutate(
      field_intact_fragment =
        ifelse(is.na(.data$intact_fragment), "missing", NA),
      field_intact_fragment =
        ifelse(!.data$intact_fragment %in% c(10, 20, 30), "not in lookuplist",
               .data$field_intact_fragment),
      field_intact_fragment =
        ifelse(
          .data$intact_fragment == 30 & .data$plottype %in% c("CP", "CA"),
          "invalid for plottype",
          .data$field_intact_fragment
        ),
      field_intact_fragment =
        ifelse(
          .data$intact_fragment == 10 & !.data$plottype %in% c("CA", "BE"),
          "invalid for plottype",
          .data$field_intact_fragment
        ),
      field_alive_dead = ifelse(.data$alive_dead == 11, "tree alive", NA),
      field_decaystage = ifelse(is.na(.data$decay_stage), "missing", NA),
      field_decaystage =
        ifelse(
          !.data$decay_stage %in% c(10, 11, 12, 13, 14, 15, 16) &
            !is.na(.data$decay_stage),
          "not in lookuplist",
          .data$field_decaystage),
      field_decaystage =
        ifelse(
          .data$decay_stage == 16 & .data$alive_dead == 12 &
            !is.na(.data$decay_stage & !is.na(.data$alive_dead)),
          "tree not alive",
          .data$field_decaystage)
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
      .data$plot_id, .data$lying_deadw_id, .data$period
    ) %>%
    summarise(
      aberrant_field = paste0(.data$aberrant_field, collapse = " / "),
      anomaly = paste0(.data$anomaly, collapse = " / ")
    ) %>%
    ungroup()

  return(incorrect_deadwood)
}
