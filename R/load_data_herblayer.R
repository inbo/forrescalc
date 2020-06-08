#' retrieve species specific vegetation data from fieldmap database
#'
#' This function queries the given database to retrieve data on vegetation (ready for use in calculate_vegetation function).
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with vegetation data on the species level ('herb layer')
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' load_data_herblayer("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows mutate
#' @importFrom lubridate year
#'
load_data_herblayer <-
  function(database, plottype = NA, forest_reserve = NA) {
    selection <-
      translate_input_to_selectionquery(database, plottype, forest_reserve)
    query_herblayer <-
      sprintf(
        "SELECT Plots.ID AS plot_id,
          Plots.Plottype AS plottype,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
          pd.ForestReserve AS forest_reserve,
          pd.LenghtCoreArea_m AS length_core_area_m,
          pd.WidthCoreArea_m AS width_core_area_m,
          Veg.ID AS subplot_id,
          Herb.ID AS measurement_id,
          Herb.Deviating_date AS deviating_date,
          Veg.Date AS date_vegetation,
          Veg.Year AS year_record,
          Herb.Species as species,
          Herb.Coverage AS coverage_id,
          qCoverHerbs.Value2 AS coverage_class_average,
          Herb.BrowseIndex AS browse_index_id
        FROM ((((Plots
          INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Vegetation Veg ON Plots.ID = Veg.IDPlots)
          INNER JOIN Herblayer Herb
            ON Veg.IDPlots = Herb.IDPlots AND Veg.Id = Herb.IDVegetation)
          INNER JOIN qCoverHerbs ON Herb.Coverage = qCoverHerbs.ID)
        %s;",
        selection
      )

    query_herblayer2 <-
      sprintf(
        "SELECT Plots.ID AS plot_id,
          Plots.Plottype AS plottype,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
          pd.ForestReserve AS forest_reserve,
          pd.LenghtCoreArea_m AS length_core_area_m,
          pd.WidthCoreArea_m AS width_core_area_m,
          Veg.ID AS subplot_id,
          Herb.ID AS measurement_id,
          Herb.Deviating_date AS deviating_date,
          Veg.Date AS date_vegetation,
          Veg.Year AS year_record,
          Herb.Species as species,
          Herb.Coverage AS coverage_id,
          qCoverHerbs.Value2 AS coverage_class_average,
          Herb.BrowseIndex AS browse_index_id
        FROM ((((Plots
          INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Vegetation_2eSet Veg ON Plots.ID = Veg.IDPlots)
          INNER JOIN Herblayer_2eSet Herb
            ON Veg.IDPlots = Herb.IDPlots AND Veg.Id = Herb.IDVegetation_2eSet)
          INNER JOIN qCoverHerbs ON Herb.Coverage = qCoverHerbs.ID)
        %s;",
        selection
      )

  con <- odbcConnectAccess2007(database)
  data_herblayer <- sqlQuery(con, query_herblayer, stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_herblayer2, stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    ) %>%
    mutate(
      year =
        ifelse(
          is.na(.data$deviating_date),
          year(.data$date_vegetation),
          year(.data$deviating_date)
        ),
      year = ifelse(is.na(.data$year), .data$year_record, .data$year),
      plotarea_ha =
        ifelse(
          .data$plottype == 20,
          0.16 * 0.16,
          NA
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == 30,
          (.data$length_core_area_m * .data$width_core_area_m)/10000,
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
    )
  odbcClose(con)

  return(data_herblayer)
}
