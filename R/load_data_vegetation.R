#' retrieve vegetation data from fieldmap database
#'
#' This function queries the given database to retrieve data on vegetation
#' (ready for use in calculate_vegetation function).
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with vegetation data, containing columns as
#' total_herb_cover, total_shrub_cover, total_tree_cover,
#' total_soildisturbance_game, date_vegetation (= date of vegetation survey),
#' year_main_survey (= year of vegetation survey), ....
#'
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' load_data_vegetation(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% left_join mutate relocate rename
#' @importFrom lubridate year
#' @importFrom tidyselect contains last_col
#' @importFrom utils packageVersion
#'
load_data_vegetation <-
  function(database, plottype = NA, forest_reserve = NA, processed = TRUE) {
    selection <-
      translate_input_to_selectquery(
        database = database, plottype = plottype,
        forest_reserve = forest_reserve, processed = processed,
        survey_name = "Survey_Vegetation_YN"
      )
    # in the below query, 'default values for columns are added to set the
    # columns in the correct order, they are overwritten later in the R script
    query_vegetation <-
        "SELECT pd.ForestReserve AS forest_reserve,
          Plots.ID AS plot_id,
          qPlotType.Value3 AS plottype,
          Veg.ID AS subplot_id,
          99 AS period,
          Veg.Year AS year_main_survey,
          Veg.Date AS date_vegetation,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha)
            AS totalplotarea_ha,
          0.0 AS plotarea_ha,
          Veg.Total_moss_cover AS total_moss_cover_id,
          Veg.Total_herb_cover AS total_herb_cover_id,
          Veg.Total_shrub_cover AS total_shrub_cover_id,
          Veg.Total_tree_cover AS total_tree_cover_id,
          Veg.Total_waterlayer_cover AS total_waterlayer_cover_id,
          Veg.Total_SoildisturbanceGame As total_soildisturbance_game_id,
          pd.LengthCoreArea_m AS length_core_area_m,
          pd.WidthCoreArea_m AS width_core_area_m,
          pd.Area_ha AS core_area_ha
        FROM (((Plots
          INNER JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Vegetation%2$s Veg ON Plots.ID = Veg.IDPlots)
          INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
        %3$s;"

    query_total_cover <-
      "SELECT tc.ID AS id,
              tc.Value1 AS cover_interval
        FROM qtotalCover tc"

  con <- connect_to_database(database)
  total_cover <- dbGetQuery(con, query_total_cover) %>%
    mutate(
      min_cover = gsub("^(\\d+) - (\\d+) %", "\\1", .data$cover_interval),
      max_cover = gsub("^(\\d+) - (\\d+) %", "\\2", .data$cover_interval),
      min_cover = ifelse(.data$min_cover == "< 1%", 0, .data$min_cover),
      max_cover = ifelse(.data$max_cover == "< 1%", 1, .data$max_cover),
      min_cover =
        ifelse(.data$min_cover == "Niet beschikbaar", NA, .data$min_cover),
      max_cover =
        ifelse(.data$max_cover == "Niet beschikbaar", NA, .data$max_cover),
      min_cover = as.numeric(.data$min_cover),
      max_cover = as.numeric(.data$max_cover)
    )
  dbDisconnect(con)

  data_vegetation <-
    query_database(database, query_vegetation, selection = selection) %>%
    group_by(.data$forest_reserve, .data$period) %>%
    mutate(
      not_na_waterlayer_cover =
        any(!is.na(.data$total_waterlayer_cover_id) &
              .data$total_waterlayer_cover_id != 20),
      not_na_soildisturbance_game =
        any(!is.na(.data$total_soildisturbance_game_id) &
              .data$total_soildisturbance_game_id != 20)
    ) %>%
    ungroup() %>%
    mutate(
      year_main_survey = ifelse(!is.na(.data$date_vegetation)
                                , as.integer(year(.data$date_vegetation))
                                , .data$year_main_survey),
      plotarea_ha =
        ifelse(
          .data$plottype == "CP",
          0.16 * 0.16,
          NA
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == "CA",
          (.data$length_core_area_m * .data$width_core_area_m) / 10000,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == "CA" & is.na(.data$plotarea_ha),
          .data$core_area_ha,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          is.na(.data$plotarea_ha),
          .data$totalplotarea_ha,
          .data$plotarea_ha
        )
    ) %>%
    left_join(total_cover, by = c("total_moss_cover_id" = "id")) %>%
    rename(
      moss_cover_interval = "cover_interval",
      moss_cover_min = "min_cover",
      moss_cover_max = "max_cover"
    ) %>%
    left_join(total_cover, by = c("total_herb_cover_id" = "id")) %>%
    rename(
      herb_cover_interval = "cover_interval",
      herb_cover_min = "min_cover",
      herb_cover_max = "max_cover"
    ) %>%
    left_join(total_cover, by = c("total_shrub_cover_id" = "id")) %>%
    rename(
      shrub_cover_interval = "cover_interval",
      shrub_cover_min = "min_cover",
      shrub_cover_max = "max_cover"
    ) %>%
    left_join(total_cover, by = c("total_tree_cover_id" = "id")) %>%
    rename(
      tree_cover_interval = "cover_interval",
      tree_cover_min = "min_cover",
      tree_cover_max = "max_cover"
    ) %>%
    left_join(total_cover, by = c("total_waterlayer_cover_id" = "id")) %>%
    rename(
      waterlayer_cover_interval = "cover_interval",
      waterlayer_cover_min = "min_cover",
      waterlayer_cover_max = "max_cover"
    ) %>%
    left_join(total_cover, by = c("total_soildisturbance_game_id" = "id")) %>%
    rename(
      soildisturbance_game_cover_interval = "cover_interval",
      soildisturbance_game_cover_min = "min_cover",
      soildisturbance_game_cover_max = "max_cover"
    ) %>%
    mutate(
      # correct for NA instead of < 1% (waterlayer and soildisturbance only)
      waterlayer_cover_interval =
        ifelse(is.na(.data$total_waterlayer_cover_id) &
                 .data$not_na_waterlayer_cover,
               "< 1%",
               .data$waterlayer_cover_interval),
      waterlayer_cover_min =
        ifelse(is.na(.data$waterlayer_cover_min) &
                 .data$not_na_waterlayer_cover,
               0,
               .data$waterlayer_cover_min),
      waterlayer_cover_max =
        ifelse(is.na(.data$waterlayer_cover_max) &
                 .data$not_na_waterlayer_cover,
               1,
               .data$waterlayer_cover_max),
      soildisturbance_game_cover_interval =
        ifelse(is.na(.data$total_soildisturbance_game_id) &
                 .data$not_na_soildisturbance_game,
               "< 1%",
               .data$soildisturbance_game_cover_interval),
      soildisturbance_game_cover_min =
        ifelse(is.na(.data$soildisturbance_game_cover_min) &
                 .data$not_na_soildisturbance_game,
               0,
               .data$soildisturbance_game_cover_min),
      soildisturbance_game_cover_max =
        ifelse(is.na(.data$soildisturbance_game_cover_max) &
                 .data$not_na_soildisturbance_game,
               1,
               .data$soildisturbance_game_cover_max),
      # calculate mid-values
      moss_cover_mid = (.data$moss_cover_min + .data$moss_cover_max) / 2,
      herb_cover_mid = (.data$herb_cover_min + .data$herb_cover_max) / 2,
      shrub_cover_mid = (.data$shrub_cover_min + .data$shrub_cover_max) / 2,
      tree_cover_mid = (.data$tree_cover_min + .data$tree_cover_max) / 2,
      waterlayer_cover_mid =
        (.data$waterlayer_cover_min + .data$waterlayer_cover_max) / 2,
      soildisturbance_game_cover_mid =
        (.data$soildisturbance_game_cover_min +
           .data$soildisturbance_game_cover_max) / 2
    ) %>%
    relocate(contains("core_area_"), .after = last_col())

  attr(data_vegetation, "database") <-
    sub("^.*\\/(.*)\\/.*\\.\\w*$", "\\1", database)
  attr(data_vegetation, "forrescalc") <-
    paste("forrescalc", packageVersion("forrescalc"))

  return(data_vegetation)
}
