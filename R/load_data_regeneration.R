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
#' library(forrescalc)
#' load_data_regeneration("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' load_data_regeneration(
#'   "C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb",
#'   plottype = "CP"
#' )
#' }
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% left_join mutate select
#' @importFrom lubridate year
#'
load_data_regeneration <-
  function(database, plottype = NA, forest_reserve = NA, processed = TRUE) {
    selection <-
      translate_input_to_selectionquery(
        database = database, plottype = plottype,
        forest_reserve = forest_reserve, processed = processed,
        survey_name = "Survey_Regeneration_YN"
      )
    conjunction <-
      ifelse(grepl("WHERE", selection), "AND", "WHERE")
    query_regeneration <-
        "SELECT Plots.ID AS plot_id,
          qPlotType.Value3 AS plottype,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
          pd.ForestReserve AS forest_reserve, pd.rA2 AS r_A2, pd.rA1 AS r_A1,
          pd.LengthCoreArea_m AS length_core_area_m,
          pd.WidthCoreArea_m AS width_core_area_m,
          pd.Area_ha AS core_area_ha,
          Reg.ID AS subplot_id,
          Reg.Date AS date_regeneration,
          Reg.Year AS year_main_survey,
          Subquery.height_class,
          Subquery.species,
          Subquery.number_class,
          Subquery.nr_of_regeneration,
          IIf(Subquery.rdn IS NULL AND pd.GameImpactRegObserved = 10 AND
              Subquery.species IS NULL,
              0, Subquery.rdn) AS rubbing_damage_number
        FROM ((((Plots INNER JOIN PlotDetails_%1$deSet AS pd ON Plots.ID = pd.IDPlots)
          INNER JOIN Regeneration%2$s AS Reg ON Plots.ID = Reg.IDPlots)
          INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
          LEFT JOIN
            (SELECT hc.HeightClass AS height_class,
              rs.Species AS species,
              rs.NumberClass AS number_class,
              rs.Number AS nr_of_regeneration,
              rs.GameDamage_number AS rdn,
              hc.IDRegeneration%2$s, hc.IDPlots
            FROM HeightClass%2$s hc INNER JOIN RegSpecies%2$s rs
                ON hc.IDRegeneration%2$s = rs.IDRegeneration%2$s
                AND hc.IDPlots = rs.IDPlots
                AND hc.ID = rs.IDHeightClass%2$s) AS Subquery
            ON Reg.ID = Subquery.IDRegeneration%2$s
            AND Reg.IDPlots = Subquery.IDPlots) %3$s
        %5$s (Reg.Date Is Not Null OR Reg.Year Is Not Null);"

  number_classes <-
    data.frame(
      id = c(1, 3, 8, 15, 30, 50, 80, 101, 1001),
      number_class =
        c("1", "2 - 5", "6 - 10", "11 - 20", "21 - 40", "41 - 60", "61 - 100", "> 100", "> 1000"),
      min_number_of_regeneration = c(1, 2, 6, 11, 21, 41, 61, 101, 1001),
      max_number_of_regeneration = c(1, 5, 10, 20, 40, 60, 100, 1000, 10000),
      stringsAsFactors = FALSE
    )

  data_regeneration <-
    query_database(database, query_regeneration,
                   selection = selection, conjunction = conjunction) %>%
    mutate(
      year = year(.data$date_regeneration),
      year = ifelse(is.na(.data$year), .data$year_main_survey, .data$year),
      subcircle =
        ifelse(
          .data$height_class %in% c(3000, 4000, 6000, 7000, 8000),
          "A2",
          ifelse(
            .data$height_class %in% c(1000, 2000, 5000),
            "A1",
            NA_character_
          )
        ),
      subcirclearea_ha =
        ifelse(
          .data$subcircle == "A2",
          (pi * .data$r_A2 ^ 2) / 10000,
          (pi * .data$r_A1 ^ 2) / 10000
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == "CP",
          .data$subcirclearea_ha,
          NA
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == "CA",
          (.data$length_core_area_m * .data$width_core_area_m) / 10000,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == "CA" & is.na(.data$plotarea_ha),
          .data$core_area_ha,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          is.na(.data$plotarea_ha),
          .data$totalplotarea_ha,
          .data$plotarea_ha
        ),
      rubbing_damage_perc = .data$rubbing_damage_number * 100 / .data$nr_of_regeneration
    ) %>%
    left_join(
      number_classes %>%
        select(-.data$number_class),
      by = c("number_class" = "id")
    ) %>%
    mutate(
      nr_of_regeneration =
        ifelse(
          is.na(.data$nr_of_regeneration) & is.na(.data$species),
          0,
          .data$nr_of_regeneration
        ),
      min_number_of_regeneration =
        ifelse(
          is.na(.data$min_number_of_regeneration),
          .data$nr_of_regeneration,
          .data$min_number_of_regeneration
        ),
      max_number_of_regeneration =
        ifelse(
          is.na(.data$max_number_of_regeneration),
          .data$nr_of_regeneration,
          .data$max_number_of_regeneration
        ),
      mid_number_of_regeneration =
        (.data$min_number_of_regeneration + .data$max_number_of_regeneration) / 2
    )

  return(data_regeneration)
}
