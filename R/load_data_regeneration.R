#' retrieve regeneration data from fieldmap database
#'
#' This function queries the given database to retrieve data on regeneration (ready for use in calculate_regeneration function).
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with regeneration data
#'
#' @examples
#' \dontrun{
#' #change path before running
#' load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows left_join mutate
#' @importFrom lubridate year
#'
load_data_regeneration <-
  function(database, plottype = NA, forest_reserve = NA) {
    if (!is.na(plottype)) {
      check_input(plottype, database, "qPlotType", "Value2")
      selection <-
        paste0(
          " INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID",
          " WHERE qPlotType.Value2 in ('", plottype, "')")
    } else {
      selection <- ""
    }
    if (!is.na(forest_reserve)) {
      check_input(forest_reserve, database, "PlotDetails_1eSet", "ForestReserve")
      if (selection == "") {
        selection <- "WHERE"
      } else {
        selection <- paste(selection, "AND")
      }
      selection <-
        paste0(selection, " pd.ForestReserve in ('", forest_reserve, "')")
    } else {
      selection <- ""
    }
    query_regeneration <-
      sprintf(
        "SELECT Plots.ID AS plot_id
          , pd.ForestReserve, Reg.Area_m2
          , Reg.Date AS date_regeneration
          , Reg.Year AS year_record
          , HeightClass.HeightClass AS height_class
          , RegSpecies.Species AS species
          , RegSpecies.NumberClass AS number_class
          , RegSpecies.Number
          , RegSpecies.GameDamage_number
        FROM ((Plots INNER JOIN PlotDetails_1eSet AS pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Regeneration AS Reg ON Plots.ID = Reg.IDPlots)
          INNER JOIN
            (HeightClass
              INNER JOIN RegSpecies
                ON HeightClass.IDRegeneration = RegSpecies.IDRegeneration
                AND HeightClass.IDPlots = RegSpecies.IDPlots
                AND HeightClass.ID = RegSpecies.IDHeightClass)
            ON Reg.ID = HeightClass.IDRegeneration
            AND Reg.IDPlots = HeightClass.IDPlots %s;",
        selection
      )


  #   Reg.Area_m2: opp. beter halen uit plotdetails: obv rA2
  # idem voor Date


    query_regeneration2 <-
      sprintf(
        "SELECT Plots.ID AS plot_id
          , pd.ForestReserve, Reg.Area_m2
          , Reg.Date AS date_regeneration
          , Reg.Year AS year_record
          , hc.HeightClass AS height_class
          , rc.Species AS species
          , rc.NumberClass AS number_class
          , rc.Number
          , rc.GameDamage_number
        FROM ((Plots INNER JOIN PlotDetails_2eSet AS pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Regeneration_2eSet AS Reg ON Plots.ID = Reg.IDPlots)
          INNER JOIN
            (HeightClass_2eSet hc
              INNER JOIN RegSpecies_2eSet rc
                ON hc.IDRegeneration_2eSet = rc.IDRegeneration_2eSet
                AND hc.IDPlots = rc.IDPlots
                AND hc.ID = rc.IDHeightClass_2eSet)
            ON Reg.ID = hc.IDRegeneration_2eSet
            AND Reg.IDPlots = hc.IDPlots %s;",
        selection
      )

  number_classes <-
    data.frame(
      id = c(1, 3, 8, 15, 30, 50, 80, 101, 1001),
      number_class =
        c("1", "2 - 5", "6 - 10", "11 - 20", "21 - 40", "41 - 60", "61 - 100", "> 100", "> 1000"),
      min_number_of_trees = c(1, 2, 6, 11, 21, 41, 61, 101, 1001),
      max_number_of_trees = c(1, 5, 10, 20, 40, 60, 100, 1000, NA),
      stringsAsFactors = FALSE
    )

  con <- odbcConnectAccess2007(database)
  data_regeneration <- sqlQuery(con, query_regeneration, stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_regeneration2, stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    ) %>%
    mutate(
      area_ha = .data$Area_m2 / 10000,
      Area_m2 = NULL,
      year = year(.data$date_regeneration),
      year = ifelse(is.na(.data$year), .data$year_record, .data$year)
    ) %>%
    left_join(
      number_classes %>%
        select(-.data$number_class),
      by = c("number_class" = "id")
    )
  odbcClose(con)

  return(data_regeneration)
}
