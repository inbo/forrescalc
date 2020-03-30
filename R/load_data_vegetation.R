#' retrieve vegetation data from fieldmap database
#'
#' This function queries the given database to retrieve data on vegetation (ready for use in calculate_vegetation function).
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with regeneration data
#'
#' @examples
#' \dontrun{
#' #change path before running
#' load_data_vegetation("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows mutate
#' @importFrom lubridate year
#'
load_data_vegetation <-
  function(database, plottype = NA, forest_reserve = NA) {
    selection <-
      translate_input_to_selectionquery(database, plottype, forest_reserve)
    query_vegetation <-
      sprintf(
        "SELECT Plots.ID AS plot_id,
          Plots.Plottype,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS Area_ha,
          pd.ForestReserve, pd.rA2,
          pd.LenghtCoreArea_m, pd.WidthCoreArea_m,
          Veg.Date AS date_vegetation,
          Veg.Year AS year_record,
          Veg.Total_moss_cover,
          Veg.Total_herb_cover,
          Veg.Total_shrub_cover,
          Veg.Total_tree_cover,
          Herb.Species as species,
          Herb.Coverage
        FROM ((Plots
          INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Vegetation Veg ON Plots.ID = Veg.IDPlots)
          INNER JOIN Herblayer Herb
            ON Veg.IDPlots = Herb.IDPlots AND Veg.Id = Herb.IDVegetation %s;",
        selection
      )

    query_vegetation2 <-
      sprintf(
        "SELECT Plots.ID AS plot_id,
          Plots.Plottype,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS Area_ha,
          pd.ForestReserve, pd.rA2,
          pd.LenghtCoreArea_m, pd.WidthCoreArea_m,
          Veg.Date AS date_vegetation,
          Veg.Year AS year_record,
          Veg.Total_moss_cover,
          Veg.Total_herb_cover,
          Veg.Total_shrub_cover,
          Veg.Total_tree_cover,
          Herb.Species as species,
          Herb.Coverage
        FROM ((Plots
          INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Vegetation_2eSet Veg ON Plots.ID = Veg.IDPlots)
          INNER JOIN Herblayer_2eSet Herb
            ON Veg.IDPlots = Herb.IDPlots AND Veg.Id = Herb.IDVegetation_2eSet
        %s;",
        selection
      )


  con <- odbcConnectAccess2007(database)
  data_vegetation <- sqlQuery(con, query_vegetation, stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_vegetation2, stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    ) %>%
    mutate(
      year = year(.data$date_vegetation),
      year = ifelse(is.na(.data$year), .data$year_record, .data$year),
      plotarea_ha =
        ifelse(
          .data$Plottype == 20,
          (pi * .data$rA2 ^ 2)/10000,
          NA
        ),
      plotarea_ha =
        ifelse(
          .data$Plottype == 30,
          .data$LenghtCoreArea_m * .data$WidthCoreArea_m,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          is.na(.data$plotarea_ha),
          .data$Area_ha,
          .data$plotarea_ha
        )
    )
  odbcClose(con)

  return(data_vegetation)
}
