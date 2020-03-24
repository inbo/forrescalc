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
#' @importFrom dplyr bind_rows mutate
#' @importFrom lubridate round_date year
#'
load_data_deadwood <-
  function(database, plottype = c(NA, "CP", "KV"), forest_reserve = NA) {
  plottypeid <- give_plottypeid(plottype)
  if (!is.na(forest_reserve)) {
    check_input(forest_reserve, database, "PlotDetails_1eSet", "ForestReserve")
    selection_fr <- paste0(" AND pd.ForestReserve in ('", forest_reserve, "')")
  } else {
    selection_fr <- ""
  }

  query_deadwood <-
    sprintf(
      "SELECT Plots.ID AS plot_id,
        Plots.Plottype,
        pd.ForestReserve,
        pd.Date_dendro_1eSet AS date_dendro,
        pd.rA1, pd.rA2, pd.rA3, pd.rA4,
        Deadwood.Species AS species,
        Deadwood.DecayStage AS decaystage,
        Deadwood.CalcVolume_m3
      FROM (Plots INNER JOIN Deadwood ON Plots.ID = Deadwood.IDPlots)
        INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots
      WHERE Plots.Plottype in (%s)%s;",
      plottypeid, selection_fr
    )

  query_deadwood2 <-
    sprintf(
      "SELECT Plots.ID AS plot_id,
        Plots.Plottype,
        pd.ForestReserve,
        pd.Date_dendro_2eSet AS date_dendro,
        pd.rA1, pd.rA2, pd.rA3, pd.rA4,
        Deadwood_2eSet.Species AS species,
        Deadwood_2eSet.DecayStage AS decaystage,
        Deadwood_2eSet.CalcVolume_m3
      FROM (Plots INNER JOIN Deadwood_2eSET ON Plots.ID = Deadwood_2eSET.IDPlots)
        INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots
      WHERE Plots.Plottype in (%s)%s;",
      plottypeid, selection_fr
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
      year = year(round_date(.data$date_dendro, "year")) - 1
    )
  odbcClose(con)

  return(data_deadwood)
}
