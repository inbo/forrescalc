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
#' @importFrom dplyr %>% bind_rows left_join mutate rename
#' @importFrom lubridate year
#'
load_data_vegetation <-
  function(database, plottype = NA, forest_reserve = NA) {
    selection <-
      translate_input_to_selectionquery(database, plottype, forest_reserve)
    query_vegetation <-
      sprintf(
        "SELECT Plots.ID AS plot_id,
          Plots.Plottype AS plottype,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
          pd.ForestReserve AS forest_reserve,
          pd.LenghtCoreArea_m AS length_core_area_m,
          pd.WidthCoreArea_m AS width_core_area_m,
          Veg.ID AS subplot_id,
          Veg.Date AS date_vegetation,
          Veg.Year AS year_record,
          Veg.Total_moss_cover AS total_moss_cover_id,
          Veg.Total_herb_cover AS total_herb_cover_id,
          Veg.Total_shrub_cover AS total_shrub_cover_id,
          Veg.Total_tree_cover AS total_tree_cover_id,
          Veg.Total_waterlayer_cover AS total_waterlayer_cover_id,
          Veg.Total_SoildisturbanceGame As total_soildisturbance_game,
          Herb.Species as species,
          Herb.Coverage AS coverage_id,
          Herb.BrowseIndex AS browse_index_id
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
          Plots.Plottype AS plottype,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
          pd.ForestReserve AS forest_reserve,
          pd.LenghtCoreArea_m AS length_core_area_m,
          pd.WidthCoreArea_m AS width_core_area_m,
          Veg.ID AS subplot_id,
          Veg.Date AS date_vegetation,
          Veg.Year AS year_record,
          Veg.Total_moss_cover AS total_moss_cover_id,
          Veg.Total_herb_cover AS total_herb_cover_id,
          Veg.Total_shrub_cover AS total_shrub_cover_id,
          Veg.Total_tree_cover AS total_tree_cover_id,
          Veg.Total_waterlayer_cover AS total_waterlayer_cover_id,
          Veg.Total_SoildisturbanceGame As total_soildisturbance_game,
          Herb.Species as species,
          Herb.Coverage AS coverage_id,
          Herb.BrowseIndex AS browse_index_id
        FROM ((Plots
          INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Vegetation_2eSet Veg ON Plots.ID = Veg.IDPlots)
          INNER JOIN Herblayer_2eSet Herb
            ON Veg.IDPlots = Herb.IDPlots AND Veg.Id = Herb.IDVegetation_2eSet
        %s;",
        selection
      )

    query_total_cover <-
      "SELECT tc.ID AS id, tc.Value1 AS cover_interval FROM qtotalCover tc"

  con <- odbcConnectAccess2007(database)
  total_cover <- sqlQuery(con, query_total_cover, stringsAsFactors = FALSE) %>%
    mutate(
      min_cover = gsub("^(\\d+) - (\\d+) %", "\\1", .data$cover_interval),
      max_cover = gsub("^(\\d+) - (\\d+) %", "\\2", .data$cover_interval),
      min_cover = ifelse(.data$min_cover == "< 1%", 0, .data$min_cover),
      max_cover = ifelse(.data$max_cover == "< 1%", 1, .data$max_cover),
      min_cover = ifelse(.data$min_cover == "nvt", NA, .data$min_cover),
      max_cover = ifelse(.data$max_cover == "nvt", NA, .data$max_cover),
      min_cover = as.numeric(.data$min_cover),
      max_cover = as.numeric(.data$max_cover)
    )
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
          .data$plottype == 20,
          0.16 * 0.16,
          NA
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == 30,
          .data$length_core_area_m * .data$width_core_area_m,
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
      moss_cover_interval = .data$cover_interval,
      moss_cover_min = .data$min_cover,
      moss_cover_max = .data$max_cover
    ) %>%
    left_join(total_cover, by = c("total_herb_cover_id" = "id")) %>%
    rename(
      herb_cover_interval = .data$cover_interval,
      herb_cover_min = .data$min_cover,
      herb_cover_max = .data$max_cover
    ) %>%
    left_join(total_cover, by = c("total_shrub_cover_id" = "id")) %>%
    rename(
      shrub_cover_interval = .data$cover_interval,
      shrub_cover_min = .data$min_cover,
      shrub_cover_max = .data$max_cover
    ) %>%
    left_join(total_cover, by = c("total_tree_cover_id" = "id")) %>%
    rename(
      tree_cover_interval = .data$cover_interval,
      tree_cover_min = .data$min_cover,
      tree_cover_max = .data$max_cover
    ) %>%
    left_join(total_cover, by = c("total_waterlayer_cover_id" = "id")) %>%
    rename(
      waterlayer_cover_interval = .data$cover_interval,
      waterlayer_cover_min = .data$min_cover,
      waterlayer_cover_max = .data$max_cover
    )
  odbcClose(con)

  return(data_vegetation)
}
