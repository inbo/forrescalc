#' retrieves dendrometry data from fieldmap database
#'
#' This function queries the given database to retrieve data on dendrometry (ready for use in calculate_dendrometry function).
#'
#' @param database name of fieldmap/access database (with specific fieldmap structure) including path
#'
#' @return Dataframe with dendrometry data
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows mutate
#' @importFrom lubridate year
#'
load_data_dendrometry <- function(database) {
  query_dendro <-
    "SELECT Plots.ID AS plot_id,
      IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS Area_ha,
      Trees.ID AS tree_id,
      Plots.Date_dendro_1eSet AS date_dendro,
      Trees.DBH_mm,
      Trees.Height_m,
      qSpecies.Value1 AS species,
      Trees.AliveDead,
      decay.Value1 AS decaystage,
      Trees.Vol_stem_m3,
      Trees.Vol_crown_m3,
      Trees.Vol_tot_m3,
      Trees.BasalArea_m2
    FROM ((Plots
      INNER JOIN Trees ON Plots.ID = Trees.IDPlots)
      LEFT JOIN qSpecies ON Trees.Species = qSpecies.ID)
      LEFT JOIN
      (
        SELECT * FROM qdecaystage
        WHERE qdecaystage.MasterID <> 16
      ) decay
      ON Trees.DecayStage = decay.ID;"

  query_dendro2 <-
    "SELECT Plots.ID AS plot_id,
      IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS Area_ha,
      Trees.ID AS tree_id,
      Plots.Date_dendro_2eSet AS date_dendro,
      Trees.DBH_mm,
      Trees.Height_m,
      qSpecies.Value1 AS species,
      Trees.AliveDead,
      decay.Value1 AS decaystage,
      Trees.Vol_stem_m3,
      Trees.Vol_crown_m3,
      Trees.Vol_tot_m3,
      Trees.BasalArea_m2,
      Trees.OldID
    FROM ((Plots
      INNER JOIN Trees_2eSET Trees ON Plots.ID = Trees.IDPlots)
      LEFT JOIN qSpecies ON Trees.Species = qSpecies.ID)
      LEFT JOIN
      (
        SELECT * FROM qdecaystage
        WHERE qdecaystage.MasterID <> 16
      ) decay
      ON Trees.DecayStage = decay.ID;"

  con <- odbcConnectAccess2007(database)
  data_dendro <- sqlQuery(con, query_dendro, stringsAsFactors = FALSE) %>%
    mutate(
      series = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_dendro2, stringsAsFactors = FALSE) %>%
        mutate(
          series = 2
        )
    ) %>%
    mutate(
      year = year(.data$date_dendro),
      alive =
        ifelse(
          .data$AliveDead == 11,
          1,
          ifelse(
            .data$AliveDead == 12,
            0,
            NA
          )
        )
    )
  odbcClose(con)

  return(data_dendro)
}
