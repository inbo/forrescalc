#' retrieve species specific vegetation data from `Fieldmap` database
#'
#' This function queries the given database to retrieve data on vegetation
#' (ready for use in `calculate_vegetation()` function).
#' `year_main_survey` refers to year of the main vegetation survey
#' (source is table `vegetation`),
#' while `year` refers to year of recording of that specific species
#' (possibly different for spring flora; source is table `herblayer`)
#'
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with vegetation data on the species level ('herb layer'),
#' containing columns as species, coverage_id, browse_index_id, date_vegetation
#' (= date of survey of specific species, different for spring flora and other
#' flora in the same plot), year (= year of survey of specific species, possibly
#' different for spring flora and other flora), ....
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' load_data_herblayer(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate select
#' @importFrom lubridate year
#' @importFrom utils packageVersion
#'
load_data_herblayer <-
  function(database, plottype = NA, forest_reserve = NA, processed = TRUE) {
    selection <-
      translate_input_to_selectquery(
        database = database, plottype = plottype,
        forest_reserve = forest_reserve, processed = processed,
        survey_name = "Survey_Vegetation_YN"
      )
    # in the below query, 'default values for columns are added to set the
    # columns in the correct order, they are overwritten later in the R script
    query_herblayer <-
        "SELECT pd.ForestReserve AS forest_reserve,
          Plots.ID AS plot_id,
          qPlotType.Value3 AS plottype,
          Veg.ID AS subplot_id,
          99 AS period,
          1234 AS year,
          IIf(Herb.Deviating_date IS NULL, Veg.Date, Herb.Deviating_date)
            AS date_vegetation,
          Veg.Year AS year_main_survey,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha)
            AS totalplotarea_ha,
          0.0 AS plotarea_ha,
          Herb.species,
          Herb.coverage_id,
          IIf(Herb.browse_index_id IS NULL AND pd.GameImpactVegObserved = 10,
            100, Herb.browse_index_id) AS browse_index_id,
          Herb.coverage_class_average,
          0.0 AS coverage_class_average_perc,
          pd.LengthCoreArea_m AS length_core_area_m,
          pd.WidthCoreArea_m AS width_core_area_m,
          pd.Area_ha AS core_area_ha
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
      year = as.integer(year(.data$date_vegetation)),
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
    select(-"year_main_survey")

  attr(data_herblayer, "database") <-
    sub("^.*\\/(.*)\\/.*\\.\\w*$", "\\1", database)
  attr(data_herblayer, "forrescalc") <-
    paste("forrescalc", packageVersion("forrescalc"))

  return(data_herblayer)
}
