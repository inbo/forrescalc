#' retrieve species specific vegetation data from fieldmap database
#'
#' This function queries the given database to retrieve data on vegetation
#' (ready for use in calculate_vegetation function). Year_main_survey refers
#' to year of the main vegetation survey (source is table "vegetation"),
#' while year refers to year of recording of that specific species
#' (possibly different for spring flora; source is table "herblayer")
#'
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with vegetation data on the species level ('herb layer'),
#' containing columns as species, coverage_id, browse_index_id, date_vegetation
#' (= date of survey of specific species, different for spring flora and other
#' flora in the same plot), year_main_survey (= year of main vegetation survey),
#' year (= year of survey of specific species, possibly different for
#' spring flora and other flora), ....
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("database/mdb_bosres.sqlite", package = "forrescalc")
#' load_data_herblayer(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
#' @importFrom lubridate year
#'
load_data_herblayer <-
  function(database, plottype = NA, forest_reserve = NA, processed = TRUE) {
    selection <-
      translate_input_to_selectionquery(
        database = database, plottype = plottype,
        forest_reserve = forest_reserve, processed = processed,
        survey_name = "Survey_Vegetation_YN"
      )
    query_herblayer <-
        "SELECT Plots.ID AS plot_id,
          qPlotType.Value3 AS plottype,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha)
            AS totalplotarea_ha,
          pd.ForestReserve AS forest_reserve,
          pd.LengthCoreArea_m AS length_core_area_m,
          pd.WidthCoreArea_m AS width_core_area_m,
          pd.Area_ha AS core_area_ha,
          Veg.ID AS subplot_id,
          IIf(Herb.Deviating_date IS NULL, Veg.Date, Herb.Deviating_date)
            AS date_vegetation,
          Veg.Year AS year_main_survey,
          Herb.species,
          Herb.coverage_id,
          Herb.coverage_class_average,
          IIf(Herb.browse_index_id IS NULL AND pd.Survey_vegetation_YN = 10,
            100, Herb.browse_index_id) AS browse_index_id
        FROM ((((Plots
          INNER JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Vegetation%2$s Veg ON Plots.ID = Veg.IDPlots)
          LEFT JOIN (
            SELECT Herblayer.IDPlots,
              Herblayer.IDVegetation%2$s,
              Herblayer.Deviating_date,
              Herblayer.Species as species,
              Herblayer.Coverage AS coverage_id,
              qCoverHerbs.Value2 AS coverage_class_average,
              Herblayer.BrowseIndex AS browse_index_id
            FROM Herblayer%2$s Herblayer
              INNER JOIN qCoverHerbs ON Herblayer.Coverage = qCoverHerbs.ID
            ) Herb
            ON Veg.IDPlots = Herb.IDPlots AND Veg.Id = Herb.IDVegetation%2$s)
          INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
        %3$s;"

  data_herblayer <-
    query_database(database, query_herblayer, selection = selection) %>%
    mutate(
      year = year(.data$date_vegetation),
      year = ifelse(is.na(.data$year), .data$year_main_survey, .data$year),
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
        ),
      coverage_class_average_perc =
        as.numeric(gsub(",", ".", .data$coverage_class_average)) * 100,
      coverage_class_average = NULL
    ) %>%
    select(-.data$year_main_survey)

  return(data_herblayer)
}
