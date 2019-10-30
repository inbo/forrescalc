#' retrieve vegetation data from fieldmap database
#'
#' This function queries the given database to retrieve data on vegetation (ready for use in calculate_vegetation function).
#'
#' @param database name of fieldmap/access database (with specific fieldmap structure) including path
#'
#' @return Dataframe with regeneration data
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows mutate
#' @importFrom lubridate year
#'
load_data_vegetation <- function(database) {
  query_vegetation <-
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
      INNER JOIN Herblayer Herb ON Veg.IDPlots = Herb.IDPlots
    WHERE Plots.Plottype = 20;"

  query_vegetation2 <-
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
      INNER JOIN Herblayer_2eSet Herb ON Veg.IDPlots = Herb.IDPlots
    WHERE Plots.Plottype = 20;"

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
