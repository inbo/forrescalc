#' check table Vegetation from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table Vegetation
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
#' check_data_vegetation(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% group_by mutate summarise ungroup
#' @importFrom tidyr pivot_longer
#'
check_data_vegetation <- function(database) {
  query_vegetation <-
    "SELECT v.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      v.ID AS subplot_id,
      v.Total_moss_cover as moss_cover_id,
      v.Total_herb_cover as herb_cover_id,
      v.Total_shrub_cover as shrub_cover_id,
      v.Total_tree_cover as tree_cover_id,
      v.Total_waterlayer_cover as waterlayer_cover_id,
      v.Total_SoildisturbanceGame as total_soildisturbance_game_id,
      v.Homogeneous as homogeneous_id
    FROM (Plots
        INNER JOIN Vegetation%2$s v ON Plots.ID = v.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_totalcover <-
    "SELECT ID as cover_id,
      Value1 as cover_interval,
      Value2 as cover_class_mean
    FROM qtotalCover;"

  data_vegetation <- query_database(database, query_vegetation)
  con <- connect_to_database(database)
  data_totalcover <- dbGetQuery(con, query_totalcover)
  dbDisconnect(con)

  incorrect_vegetation <- data_vegetation %>%
    mutate(
      field_total_moss_cover =
        ifelse(is.na(.data$moss_cover_id), "missing", NA),
      field_total_moss_cover =
        ifelse(
          !is.na(.data$moss_cover_id) &
            !.data$moss_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_total_moss_cover
        ),
      field_total_herb_cover =
        ifelse(is.na(.data$herb_cover_id), "missing", NA),
      field_total_herb_cover =
        ifelse(
          !is.na(.data$herb_cover_id) &
            !.data$herb_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_total_herb_cover
        ),
      field_total_shrub_cover =
        ifelse(is.na(.data$shrub_cover_id), "missing", NA),
      field_total_shrub_cover =
        ifelse(
          !is.na(.data$shrub_cover_id) &
            !.data$shrub_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_total_shrub_cover
        ),
      field_total_tree_cover =
        ifelse(is.na(.data$tree_cover_id), "missing", NA),
      field_total_tree_cover =
        ifelse(
          !is.na(.data$tree_cover_id) &
            !.data$tree_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_total_tree_cover
        ),
      field_total_waterlayer_cover =
        ifelse(is.na(.data$waterlayer_cover_id), "missing", NA),
      field_total_waterlayer_cover =
        ifelse(
          !is.na(.data$waterlayer_cover_id) &
            !.data$waterlayer_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_total_waterlayer_cover
        ),
      field_total_soildisturbance_game =
        ifelse(is.na(.data$total_soildisturbance_game_id), "missing", NA),
      field_total_soildisturbance_game =
        ifelse(
          !is.na(.data$total_soildisturbance_game_id) &
            !.data$total_soildisturbance_game_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_total_soildisturbance_game
        ),
      field_homogeneous = ifelse(is.na(.data$homogeneous_id), "missing", NA),
      field_homogeneous =
        ifelse(
          !is.na(.data$homogeneous_id) & !.data$homogeneous_id %in% c(10, 20),
          "not in lookuplist", .data$field_homogeneous
        )
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
      .data$plot_id, .data$subplot_id, .data$period
    ) %>%
    summarise(
      aberrant_field = paste0(.data$aberrant_field, collapse = " / "),
      anomaly = paste0(.data$anomaly, collapse = " / ")
    ) %>%
    ungroup()

  return(incorrect_vegetation)
}
