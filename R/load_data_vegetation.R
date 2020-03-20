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
  function(database, plottype = c(NA, "CP", "KV"), forest_reserve = NA) {
    plottypeid <- give_plottypeid(plottype)
    if (!is.na(forest_reserve)) {
      check_input(forest_reserve, database, "PlotDetails_1eSet", "ForestReserve")
      selection_fr <- paste0(" AND pd.ForestReserve in ('", forest_reserve, "')")
    } else {
      selection_fr <- ""
    }
    query_vegetation <-
      sprintf(
        "SELECT Plots.ID AS plot_id,
          pd.ForestReserve,
          Veg.Area_m2,
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
            ON Veg.IDPlots = Herb.IDPlots AND Veg.Id = Herb.IDVegetation
        WHERE Plots.Plottype in (%s)%s;",
        plottypeid, selection_fr
      )

    query_vegetation2 <-
      sprintf(
        "SELECT Plots.ID AS plot_id,
          pd.ForestReserve,
          Veg.Area_m2,
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
        WHERE Plots.Plottype in (%s)%s;",
        plottypeid, selection_fr
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
      area_ha = .data$Area_m2 / 10000,
      Area_m2 = NULL,
      year = year(.data$date_vegetation),
      year = ifelse(is.na(.data$year), .data$year_record, .data$year)
    )
  odbcClose(con)

  return(data_vegetation)
}
