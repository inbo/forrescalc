#' check table plots from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table plots from the given
#' database and checks for missing data or wrong input.
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
#' check_data_plots(path_to_fieldmapdb)
#' check_data_plots(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% distinct mutate select transmute
#' @importFrom tidyr pivot_longer
#'
check_data_plots <- function(database, forest_reserve = "all") {
  selection <-
    ifelse(
      forest_reserve == "all", "",
      paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
    )
  query_plots <-
    "SELECT Plots.ID As plot_id,
      Plots.Plottype AS plottype_id,
      Plots.Homogeneous AS homogeneous_id
    FROM Plots
      INNER JOIN Plotdetails_%1$deSet pd ON Plots.ID = pd.IDPlots
    %3$s;"

  query_plottype <-
    "SELECT ID as plottype_id
    FROM qPlotType;"

  data_plots <- query_database(database, query_plots, selection = selection) %>%
    select(-"period") %>%
    distinct()

  con <- connect_to_database(database)
  data_plottype <- dbGetQuery(con, query_plottype)
  dbDisconnect(con)

  incorrect_plots <- data_plots %>%
    mutate(
      field_plottype = ifelse(is.na(.data$plottype_id), "missing", NA),
      field_plottype =
        ifelse(
          !is.na(.data$plottype_id) &
            !.data$plottype_id %in% data_plottype$plottype_id,
          "not in lookuplist",
          .data$field_plottype
        ),
      field_homogeneous = ifelse(is.na(.data$homogeneous_id), "missing", NA),
      field_homogeneous =
        ifelse(
          !is.na(.data$homogeneous_id) & !.data$homogeneous_id %in% c(10, 20),
          "not in lookuplist",
          .data$field_homogeneous
        )
    ) %>%
    pivot_longer(
      cols = c(starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    transmute(
      .data$plot_id,
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      .data$anomaly
    )

  return(incorrect_plots)
}
