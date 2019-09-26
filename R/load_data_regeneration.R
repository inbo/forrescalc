#' retrieves regeneration data from fieldmap database
#'
#' This function queries the given database to retrieve data on regeneration (ready for use in calculate_dendrometry function).
#'
#' @param database name of fieldmap/access database (with specific fieldmap structure) including path
#'
#' @return Dataframe with regeneration data
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows mutate
#' @importFrom lubridate year
#'
load_data_regeneration <- function(database) {
  query_regeneration <-
    "SELECT Plots.ID AS plot_id,
      Reg.Area_m2 / 10000 AS area_ha,
      Plots.Date_vegetation_1eSet AS date_regeneration,
      qHCr.Value1 AS height_class,
      qSpecies.Value1 AS species,
      qnum.Value1 AS number_class,
      RegSpecies.GameDamage_number
    FROM (Plots
      INNER JOIN Regeneration Reg ON Plots.ID = Reg.IDPlots)
      INNER JOIN ((HeightClass INNER JOIN qHeightClass_regenaration qHCr
          ON HeightClass.HeightClass = qHCr.ID)
        INNER JOIN
          ((RegSpecies INNER JOIN qSpecies on RegSpecies.Species = qSpecies.ID)
          INNER JOIN qnumber_regeneration_classes qnum ON RegSpecies.Number = qnum.ID)
          ON HeightClass.ID = RegSpecies.IDHeightClass)
        ON Reg.ID = HeightClass.IDRegeneration;"

  query_regeneration2 <-
    "SELECT Plots.ID AS plot_id,
      Reg.Area_m2 / 10000 AS area_ha,
      Plots.Date_vegetation_1eSet AS date_regeneration,
      qHCr.Value1 AS height_class,
      qSpecies.Value1 AS species,
      qnum.Value1 AS number_class,
      RegSpecies_2eSet.GameDamage_number
    FROM (Plots
      INNER JOIN Regeneration_2eSet Reg ON Plots.ID = Reg.IDPlots)
      INNER JOIN ((HeightClass_2eSet INNER JOIN qHeightClass_regenaration qHCr
          ON HeightClass_2eSet.HeightClass = qHCr.ID)
        INNER JOIN
          ((RegSpecies_2eSet INNER JOIN qSpecies on RegSpecies_2eSet.Species = qSpecies.ID)
          INNER JOIN qnumber_regeneration_classes qnum ON RegSpecies_2eSet.Number = qnum.ID)
          ON HeightClass_2eSet.ID = RegSpecies_2eSet.IDHeightClass_2eSet)
        ON Reg.ID = HeightClass_2eSet.IDRegeneration_2eSet;"

  con <- odbcConnectAccess2007(database)
  data_regeneration <- sqlQuery(con, query_regeneration, stringsAsFactors = FALSE) %>%
    mutate(
      series = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_regeneration2, stringsAsFactors = FALSE) %>%
        mutate(
          series = 2
        )
    ) %>%
    mutate(
      year = year(.data$date_regeneration)
    )
  odbcClose(con)

  return(data_regeneration)
}
