#' check tables from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of tables Trees, Shoots,
#' Deadwood, Regeneration, Regspecies, Vegetation, Herblayer, Plots and
#' Plotdetails (of all periods) from the given database and
#' checks for missing data or wrong input.
#'
#' @inheritParams check_data_trees
#'
#' @return Dataframe with inconsistent data with table name, ID's and
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
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows mutate
#'
check_data_fmdb <- function(database) {
  incorrect_data <- check_data_trees(database) %>%
    mutate(table_name = "Trees") %>%
    bind_rows(
      check_data_shoots(database) %>%
        mutate(table_name = "Shoots")
    ) %>%
    bind_rows(
      check_data_shoots(database) %>%
        mutate(table_name = "Shoots")
    ) %>%
    bind_rows(
      check_data_deadwood(database) %>%
        mutate(table_name = "Deadwood")
    ) %>%
    bind_rows(
      check_data_regeneration(database) %>%
        mutate(table_name = "Regeneration")
    ) %>%
    bind_rows(
      check_data_regspecies(database) %>%
        mutate(table_name = "Regspecies")
    ) %>%
    bind_rows(
      check_data_vegetation(database) %>%
        mutate(table_name = "Vegetation")
    ) %>%
    bind_rows(
      check_data_herblayer(database) %>%
        mutate(table_name = "Herblayer")
    ) %>%
    bind_rows(
      check_data_plots(database) %>%
        mutate(table_name = "Plots")
    ) %>%
    bind_rows(
      check_data_plotdetails(database) %>%
        mutate(table_name = "Plotdetails")
    ) %>%
    mutate(
      tree_measure_id = as.character(.data$tree_measure_id)
    )
    bind_rows(
      check_trees_evolution(database) %>%
        mutate(table_name = "Trees diff periods")
    )

  return(incorrect_data)
}
