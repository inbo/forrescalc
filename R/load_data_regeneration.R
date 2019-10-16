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
#' @importFrom dplyr bind_rows left_join mutate
#' @importFrom lubridate year
#'
load_data_regeneration <- function(database) {
  query_regeneration <-
    "SELECT Plots.ID AS plot_id,
      Reg.Area_m2,
      Plots.Date_vegetation_1eSet AS date_regeneration,
      qHCr.Value1 AS height_class,
      qSpecies.Value1 AS species,
      qnum.Value1 AS number_class,
      RegSpecies.Number,
      RegSpecies.GameDamage_number
    FROM (Plots
      INNER JOIN Regeneration Reg ON Plots.ID = Reg.IDPlots)
      INNER JOIN ((HeightClass INNER JOIN qHeightClass_regenaration qHCr
          ON HeightClass.HeightClass = qHCr.ID)
        INNER JOIN
          ((RegSpecies INNER JOIN qSpecies on RegSpecies.Species = qSpecies.ID)
          INNER JOIN qnumber_regeneration_classes qnum ON RegSpecies.NumberClass = qnum.ID)
          ON HeightClass.ID = RegSpecies.IDHeightClass)
        ON Reg.IDPlots = HeightClass.IDPlots;"

  query_regeneration2 <-
    "SELECT Plots.ID AS plot_id,
      Reg.Area_m2,
      Plots.Date_vegetation_2eSet AS date_regeneration,
      qHCr.Value1 AS height_class,
      qSpecies.Value1 AS species,
      qnum.Value1 AS number_class,
      RegSpecies_2eSet.Number,
      RegSpecies_2eSet.GameDamage_number
    FROM (Plots
      INNER JOIN Regeneration_2eSet Reg ON Plots.ID = Reg.IDPlots)
      INNER JOIN ((HeightClass_2eSet INNER JOIN qHeightClass_regenaration qHCr
          ON HeightClass_2eSet.HeightClass = qHCr.ID)
        INNER JOIN
          ((RegSpecies_2eSet INNER JOIN qSpecies on RegSpecies_2eSet.Species = qSpecies.ID)
          INNER JOIN qnumber_regeneration_classes qnum ON RegSpecies_2eSet.NumberClass = qnum.ID)
          ON HeightClass_2eSet.ID = RegSpecies_2eSet.IDHeightClass_2eSet)
        ON Reg.IDPlots = HeightClass_2eSet.IDPlots;"

  number_classes <-
    data.frame(
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
      year = year(.data$date_regeneration)
    ) %>%
    left_join(number_classes, by = "number_class")
  odbcClose(con)

  return(data_regeneration)
}
