#' check table regeneration from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table regeneration
#' (of all periods) from the given database and
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
#' check_data_regeneration(path_to_fieldmapdb)
#' check_data_regeneration(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate transmute
#' @importFrom tidyr pivot_longer
#'
check_data_regeneration <- function(database, forest_reserve = "all") {
  selection <-
    ifelse(
      forest_reserve == "all", "",
      paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
    )
  query_regeneration <-
    "SELECT g.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      g.ID AS subplot_id,
      g.Date AS date,
      g.Fieldteam AS fieldteam
    FROM ((Plots
        INNER JOIN Regeneration%2$s g ON Plots.ID = g.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_%1$deSet pd ON Plots.ID = pd.IDPlots
    %3$s;"

  data_regeneration <-
    query_database(database, query_regeneration, selection = selection)

  incorrect_regeneration <- data_regeneration %>%
    mutate(
      field_date = ifelse(is.na(.data$date), "missing", NA),
      field_fieldteam = ifelse(is.na(.data$fieldteam), "missing", NA)
    ) %>%
    pivot_longer(
      cols = c(starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    transmute(
      .data$plot_id, .data$subplot_id, .data$period,
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      .data$anomaly
    )

  return(incorrect_regeneration)
}
