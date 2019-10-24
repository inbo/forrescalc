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
#' @importFrom dplyr %>% bind_rows left_join mutate
#' @importFrom lubridate year
#'
load_data_regeneration <- function(database) {
  query_regeneration <-
    "SELECT Plots.ID AS plot_id,
      pd.ForestReserve,
      Reg.Area_m2,
      Reg.Date AS date_regeneration,
      Reg.Year AS year_record,
      HeightClass.HeightClass AS height_class,
      RegSpecies.Species AS species,
      RegSpecies.NumberClass AS number_class,
      RegSpecies.Number,
      RegSpecies.GameDamage_number
    FROM ((Plots
      INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots)
      INNER JOIN Regeneration Reg ON Plots.ID = Reg.IDPlots)
      INNER JOIN (HeightClass INNER JOIN RegSpecies
          ON HeightClass.ID = RegSpecies.IDHeightClass)
        ON Reg.IDPlots = HeightClass.IDPlots
    WHERE Plots.Plottype = 20;"

  query_regeneration2 <-
    "SELECT Plots.ID AS plot_id,
      pd.ForestReserve,
      Reg.Area_m2,
      Reg.Date AS date_regeneration,
      Reg.Year AS year_record,
      HeightClass_2eSet.HeightClass AS height_class,
      RegSpecies_2eSet.Species AS species,
      RegSpecies_2eSet.NumberClass AS number_class,
      RegSpecies_2eSet.Number,
      RegSpecies_2eSet.GameDamage_number
    FROM ((Plots
      INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots)
      INNER JOIN Regeneration_2eSet Reg ON Plots.ID = Reg.IDPlots)
      INNER JOIN (HeightClass_2eSet INNER JOIN RegSpecies_2eSet
              ON HeightClass_2eSet.ID = RegSpecies_2eSet.IDHeightClass_2eSet)
        ON Reg.IDPlots = HeightClass_2eSet.IDPlots
    WHERE Plots.Plottype = 20;"

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
