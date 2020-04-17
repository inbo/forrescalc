#' retrieves data on logs from fieldmap database
#'
#' This function queries the given database to retrieve data on deadwood (logs) (ready for use in calculate_dendrometry function).
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with data on logs
#'
#' @examples
#' \dontrun{
#' #change path before running
#' load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows mutate
#' @importFrom lubridate round_date year
#'
load_data_deadwood <-
  function(database, plottype = NA, forest_reserve = NA) {
    selection <-
      translate_input_to_selectionquery(database, plottype, forest_reserve)

  query_deadwood <-
    sprintf(
      "SELECT Plots.ID AS plot_id,
        Plots.Plottype AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        pd.ForestReserve AS forest_reserve,
        pd.Date_dendro_1eSet AS date_dendro,
        pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
        pd.LenghtCoreArea_m AS length_core_area_m,
        pd.WidthCoreArea_m AS width_core_area_m,
        Deadwood.Species AS species,
        Deadwood.DecayStage AS decaystage,
        Deadwood.CalcVolume_m3 AS calc_volume_m3,
        Deadwood.MaxDiam_mm AS max_diam_mm,
        Deadwood.TreeNumber AS tree_number
      FROM (Plots INNER JOIN Deadwood ON Plots.ID = Deadwood.IDPlots)
        INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots %s;",
      selection
    )

  query_deadwood2 <-
    sprintf(
      "SELECT Plots.ID AS plot_id,
        Plots.Plottype AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        pd.ForestReserve AS forest_reserve,
        pd.Date_dendro_2eSet AS date_dendro,
        pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
        pd.LenghtCoreArea_m AS length_core_area_m,
        pd.WidthCoreArea_m AS width_core_area_m,
        Deadwood_2eSet.Species AS species,
        Deadwood_2eSet.DecayStage AS decaystage,
        Deadwood_2eSet.CalcVolume_m3 AS calc_volume_m3,
        Deadwood_2eSet.MaxDiam_mm AS max_diam_mm,
        Deadwood_2eSet.TreeNumber AS tree_number
      FROM (Plots INNER JOIN Deadwood_2eSET ON Plots.ID = Deadwood_2eSET.IDPlots)
        INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots %s;",
      selection
    )

  con <- odbcConnectAccess2007(database)
  data_deadwood <- sqlQuery(con, query_deadwood, stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_deadwood2, stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    ) %>%
    mutate(
      year = year(round_date(.data$date_dendro, "year")) - 1,
      dbh_class_5cm = give_diamclass_5cm(.data$max_diam_mm),
      plotarea_ha =
        ifelse(
          .data$plottype == 20,
          (pi * .data$r_A4 ^ 2)/10000,
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
    )
  odbcClose(con)

  return(data_deadwood)
}
