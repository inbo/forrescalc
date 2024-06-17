#' check table `Regeneration` from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table `Regeneration`
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
#' @importFrom dplyr %>% group_by filter mutate select ungroup
#' @importFrom lubridate year
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
      pd.ForestReserve AS forest_reserve,
      g.ID AS subplot_id,
      g.Date AS date_,
      g.Fieldteam AS fieldteam
    FROM ((Plots
        INNER JOIN Regeneration%2$s g ON Plots.ID = g.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_%1$deSet pd ON Plots.ID = pd.IDPlots
    %3$s;"

  data_regeneration <-
    query_database(database, query_regeneration, selection = selection)

  incorrect_regeneration <- data_regeneration %>%
    group_by(.data$forest_reserve, .data$period, .data$plottype) %>%
    mutate(
      forest_reserve_date = median(.data$date_)
    ) %>%
    ungroup() %>%
    mutate(
      field_date = ifelse(is.na(.data$date_), "missing", NA),
      field_date =
        ifelse(
          is.na(.data$field_date) &
            year(.data$date_) != year(.data$forest_reserve_date),
          "deviating",
          .data$field_date
        ),
      field_fieldteam = ifelse(is.na(.data$fieldteam), "missing", NA)
    ) %>%
    pivot_longer(
      cols = c(starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      plottype = NULL,
      date = as.character(.data$date_),
      date_ = NULL,
      forest_reserve_date = NULL,
      fieldteam = as.character(.data$fieldteam)
    ) %>%
    pivot_longer(
      cols = !c("plot_id", "subplot_id", "period", "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(
      .data$aberrant_field == .data$varname
    ) %>%
    select(-"varname")

  return(incorrect_regeneration)
}
