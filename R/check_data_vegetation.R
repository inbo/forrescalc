#' check table `Vegetation` from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table `Vegetation`
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
#' check_data_vegetation(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% across filter group_by mutate rename select ungroup
#' @importFrom lubridate year
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect contains ends_with
#'
check_data_vegetation <- function(database, forest_reserve = "all") {
  selection <-
    ifelse(
      forest_reserve == "all", "",
      paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
    )
  query_vegetation <-
    "SELECT v.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      pd.ForestReserve AS forest_reserve,
      v.ID AS subplot_id,
      v.Date AS date_,
      v.Fieldteam AS fieldteam,
      v.Total_moss_cover as moss_cover_id,
      v.Total_herb_cover as herb_cover_id,
      v.Total_shrub_cover as shrub_cover_id,
      v.Total_tree_cover as tree_cover_id,
      v.Total_waterlayer_cover as waterlayer_cover_id,
      v.Total_SoildisturbanceGame as total_soildisturbance_game_id
    FROM ((Plots
        INNER JOIN Vegetation%2$s v ON Plots.ID = v.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_%1$deSet pd ON Plots.ID = pd.IDPlots
    %3$s;"

  query_totalcover <-
    "SELECT ID as cover_id,
      Value1 as cover_interval,
      Value2 as cover_class_mean
    FROM qtotalCover;"

  data_vegetation <-
    query_database(database, query_vegetation, selection = selection) %>%
    rename(date = "date_")
  con <- connect_to_database(database)
  data_totalcover <- dbGetQuery(con, query_totalcover)
  dbDisconnect(con)

  incorrect_vegetation <- data_vegetation %>%
    group_by(.data$plot_id, .data$period) %>%
    mutate(
      not_na_soildisturbance_game =
        any(!is.na(.data$total_soildisturbance_game_id))
    ) %>%
    ungroup() %>%
    group_by(.data$forest_reserve, .data$period, .data$plottype) %>%
    mutate(
      forrest_reserve_date = median(.data$date)
    ) %>%
    ungroup() %>%
    mutate(
      field_date = ifelse(is.na(.data$date), "missing", NA),
      field_date =
        ifelse(
          is.na(.data$field_date) &
            year(.data$date) != year(.data$forrest_reserve_date),
          "deviating",
          .data$field_date
        ),
      field_fieldteam = ifelse(is.na(.data$fieldteam), "missing", NA),
      field_moss_cover_id =
        ifelse(is.na(.data$moss_cover_id), "missing", NA),
      field_moss_cover_id =
        ifelse(
          !is.na(.data$moss_cover_id) &
            !.data$moss_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_moss_cover_id
        ),
      field_moss_cover_id =
        ifelse(
          !is.na(.data$moss_cover_id) & .data$moss_cover_id == 20,
          "invalid value",
          .data$field_moss_cover_id
        ),
      field_herb_cover_id =
        ifelse(is.na(.data$herb_cover_id), "missing", NA),
      field_herb_cover_id =
        ifelse(
          !is.na(.data$herb_cover_id) &
            !.data$herb_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_herb_cover_id
        ),
      field_herb_cover_id =
        ifelse(
          !is.na(.data$herb_cover_id) & .data$herb_cover_id == 20,
          "invalid value",
          .data$field_herb_cover_id
        ),
      field_shrub_cover_id =
        ifelse(is.na(.data$shrub_cover_id), "missing", NA),
      field_shrub_cover_id =
        ifelse(
          !is.na(.data$shrub_cover_id) &
            !.data$shrub_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_shrub_cover_id
        ),
      field_shrub_cover_id =
        ifelse(
          !is.na(.data$shrub_cover_id) & .data$shrub_cover_id == 20,
          "invalid value",
          .data$field_shrub_cover_id
        ),
      field_tree_cover_id =
        ifelse(is.na(.data$tree_cover_id), "missing", NA),
      field_tree_cover_id =
        ifelse(
          !is.na(.data$tree_cover_id) &
            !.data$tree_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_tree_cover_id
        ),
      field_tree_cover_id =
        ifelse(
          !is.na(.data$tree_cover_id) & .data$tree_cover_id == 20,
          "invalid value",
          .data$field_tree_cover_id
        ),
      field_waterlayer_cover_id =
        ifelse(is.na(.data$waterlayer_cover_id), "missing", NA),
      field_waterlayer_cover_id =
        ifelse(
          !is.na(.data$waterlayer_cover_id) &
            !.data$waterlayer_cover_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_waterlayer_cover_id
        ),
      field_waterlayer_cover_id =
        ifelse(
          !is.na(.data$waterlayer_cover_id) & .data$waterlayer_cover_id == 20,
          "invalid value",
          .data$field_waterlayer_cover_id
        ),
      field_total_soildisturbance_game_id =
        ifelse(
          is.na(.data$total_soildisturbance_game_id) &
            .data$not_na_soildisturbance_game,
          "missing", NA
        ),
      field_total_soildisturbance_game_id =
        ifelse(
          !is.na(.data$total_soildisturbance_game_id) &
            !.data$total_soildisturbance_game_id %in% data_totalcover$cover_id,
          "not in lookuplist",
          .data$field_total_soildisturbance_game_id
        ),
      field_total_soildisturbance_game_id =
        ifelse(
          !is.na(.data$total_soildisturbance_game_id) &
            .data$total_soildisturbance_game_id == 20,
          "invalid value",
          .data$field_total_soildisturbance_game_id
        ),
      across(ends_with("date"), as.character),
      fieldteam = as.character(.data$fieldteam),
      across(ends_with("_cover_id"), as.character),
      across(contains("soildisturbance_game"), as.character)
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
        !c("plot_id", "subplot_id", "period", "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(.data$aberrant_field == .data$varname) %>%
    select(-"varname")

  return(incorrect_vegetation)
}
