#' check table herblayer from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table herblayer
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
#' check_data_herblayer(path_to_fieldmapdb)
#' check_data_herblayer(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter group_by mutate select ungroup
#' @importFrom tidyr pivot_longer
#'
check_data_herblayer <- function(database, forest_reserve = "all") {
  selection <-
    ifelse(
      forest_reserve == "all", "",
      paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
    )
  query_herblayer <-
    "SELECT hl.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      hl.IDVegetation%2$s AS subplot_id,
      hl.ID AS herblayer_id,
      hl.Species AS species,
      hl.Coverage AS coverage_id,
      hl.BrowseIndex AS browse_index
    FROM ((Plots
        INNER JOIN Herblayer%2$s hl ON Plots.ID = hl.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_%1$deSet pd ON Plots.ID = pd.IDPlots
    %3$s;"

  query_coverherbs <-
    "SELECT ID as cover_id,
      Value1 as cover_interval,
      Value2 as cover_class_mean
    FROM qCoverHerbs;"

  query_browseindex <-
    "SELECT ID as browse_index_id
    FROM qBrowsIndex;"

  data_herblayer <-
    query_database(database, query_herblayer, selection = selection)
  con <- connect_to_database(database)
  data_coverherbs <- dbGetQuery(con, query_coverherbs)
  data_browseindex <- dbGetQuery(con, query_browseindex)
  dbDisconnect(con)

  incorrect_herblayer <- data_herblayer %>%
    group_by(.data$plot_id, .data$subplot_id, .data$period, .data$species) %>%
    mutate(
      n_records = n()
    ) %>%
    ungroup() %>%
    group_by(.data$plot_id, .data$period) %>%
    mutate(
      not_na_browse_index = any(!is.na(.data$browse_index))
    ) %>%
    ungroup() %>%
    mutate(
      field_coverage_id =
        ifelse(is.na(.data$coverage_id), "missing", NA),
      field_coverage_id =
        ifelse(
          !is.na(.data$coverage_id) &
            !.data$coverage_id %in% data_coverherbs$cover_id,
          "not in lookuplist",
          .data$field_coverage_id
        ),
      field_species =
        ifelse(
          .data$n_records > 1,
          paste0(.data$n_records, " times the same species"),
          NA
        ),
      n_records = NULL,
      field_browse_index =
        ifelse(
          is.na(.data$browse_index) & .data$not_na_browse_index, "missing", NA
        ),
      field_browse_index =
        ifelse(
          !is.na(.data$browse_index) &
            !.data$browse_index %in% data_browseindex$browse_index_id,
          "not in lookuplist",
          .data$field_browse_index
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
        !c("plot_id", "subplot_id", "herblayer_id", "period", "aberrant_field",
           "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(.data$aberrant_field == .data$varname) %>%
    select(-"varname")

  return(incorrect_herblayer)
}
