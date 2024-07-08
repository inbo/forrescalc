#' check tables from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of tables Trees, Shoots,
#' Deadwood, Regeneration, Regspecies, Vegetation, Herblayer, Plots and
#' Plotdetails (of all periods) from the given database and
#' checks for missing data or wrong input.
#'
#' @inheritParams check_data_trees
#'
#' @return Dataframe with inconsistent data with layer, ID's and
#' additional columns
#' `aberrant_field` (which column is wrong) and `anomaly` (what is wrong with
#' the input)
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' check_data_fmdb(path_to_fieldmapdb)
#' check_data_fmdb(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows mutate
#'
check_data_fmdb <- function(database, forest_reserve = "all") {
  incorrect_data <- check_data_trees(database, forest_reserve) %>%
    mutate(
      layer = "Trees",
      aberrant_value = as.character(.data$aberrant_value)
    ) %>%
    bind_rows(
      check_data_shoots(database, forest_reserve) %>%
        mutate(
          layer = "Shoots",
          anomaly = as.character(.data$anomaly),
          tree_measure_id = as.character(.data$tree_measure_id),
          aberrant_value = as.character(.data$aberrant_value)
        )
    ) %>%
    bind_rows(
      check_data_deadwood(database, forest_reserve) %>%
        mutate(
          layer = "Deadwood",
          anomaly = as.character(.data$anomaly),
          aberrant_value = as.character(.data$aberrant_value)
        )
    ) %>%
    bind_rows(
      check_data_regeneration(database, forest_reserve) %>%
        mutate(
          layer = "Regeneration",
          anomaly = as.character(.data$anomaly),
          aberrant_value = as.character(.data$aberrant_value)
        )
    ) %>%
    bind_rows(
      check_data_regspecies(database, forest_reserve) %>%
        mutate(
          layer = "Regspecies",
          anomaly = as.character(.data$anomaly),
          aberrant_value = as.character(.data$aberrant_value)
        )
    ) %>%
    bind_rows(
      check_data_vegetation(database, forest_reserve) %>%
        mutate(
          layer = "Vegetation",
          anomaly = as.character(.data$anomaly),
          aberrant_value = as.character(.data$aberrant_value)
        )
    ) %>%
    bind_rows(
      check_data_herblayer(database, forest_reserve) %>%
        mutate(
          layer = "Herblayer",
          anomaly = as.character(.data$anomaly),
          aberrant_value = as.character(.data$aberrant_value)
        )
    ) %>%
    bind_rows(
      check_data_plots(database, forest_reserve) %>%
        mutate(
          layer = "Plots",
          anomaly = as.character(.data$anomaly),
          aberrant_value = as.character(.data$aberrant_value)
        )
    ) %>%
    bind_rows(
      check_data_plotdetails(database, forest_reserve) %>%
        mutate(
          layer = "Plotdetails",
          anomaly = as.character(.data$anomaly),
          aberrant_value = as.character(.data$aberrant_value)
        )
    ) %>%
    mutate(
      period = as.character(.data$period)
    ) %>%
    bind_rows(
      check_trees_evolution(database, forest_reserve) %>%
        mutate(
          layer = "Trees diff periods",
          anomaly = as.character(.data$anomaly)
        )
    )

  return(incorrect_data)
}
