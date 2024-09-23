#' retrieve regeneration data from `Fieldmap` database
#'
#' This function queries the given database to retrieve data on regeneration
#' (ready for use in `calculate_regeneration()` function).
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with regeneration data
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' load_data_regeneration(path_to_fieldmapdb)
#' load_data_regeneration(path_to_fieldmapdb, plottype = "CP")
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% left_join mutate relocate select
#' @importFrom lubridate year
#' @importFrom tidyselect ends_with
#' @importFrom utils packageVersion
#'
load_data_regeneration <-
  function(database, plottype = NA, forest_reserve = NA, processed = TRUE) {
    selection <-
      translate_input_to_selectquery(
        database = database, plottype = plottype,
        forest_reserve = forest_reserve, processed = processed,
        survey_name = "Survey_Regeneration_YN"
      )
    conjunction <-
      ifelse(grepl("WHERE", selection), "AND", "WHERE")
    # in the below query, 'default values for columns are added to set the
    # columns in the correct order, they are overwritten later in the R script
    query_regeneration <-
        "SELECT pd.ForestReserve AS forest_reserve,
          Plots.ID AS plot_id,
          qPlotType.Value3 AS plottype,
          Reg.ID AS subplot_id,
          99 AS period,
          1234 AS year,
          Reg.Date AS date_regeneration,
          Reg.Year AS year_main_survey,
          IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha)
            AS totalplotarea_ha,
          0.0 AS plotarea_ha,
          Subquery.height_class,
          Subquery.species,
          IIf(Subquery.number_class IS NULL AND
              pd.Survey_Regeneration_YN = 10 AND Subquery.species IS NULL,
              0, Subquery.number_class) AS number_class,
          IIf(Subquery.nr_of_regeneration IS NULL AND
              pd.Survey_Regeneration_YN = 10 AND Subquery.species IS NULL,
              0, Subquery.nr_of_regeneration) AS nr_of_regeneration,
          IIf(Subquery.rdn IS NULL AND pd.GameImpactRegObserved = 10,
              0, Subquery.rdn) AS rubbing_damage_number,
          0.0 AS rubbing_damage_perc,
          0 AS subcircle,
          0 AS subcirclearea_ha,
          pd.rA1 AS r_A1, pd.rA2 AS r_A2,
          pd.LengthCoreArea_m AS length_core_area_m,
          pd.WidthCoreArea_m AS width_core_area_m,
          pd.Area_ha AS core_area_ha
        FROM ((((Plots
          INNER JOIN PlotDetails_%1$deSet AS pd ON Plots.ID = pd.IDPlots)
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
      id = c(1, 3, 8, 15, 30, 50, 80, 101, 1001, 0),
      number_class =
        c("1", "2 - 5", "6 - 10", "11 - 20", "21 - 40", "41 - 60", "61 - 100",
          "> 100", "> 1000", "0"),
      approx_nr_regeneration = c(1, 3, 8, 15, 30, 50, 80, 101, 1001, 0),
      min_number_of_regeneration = c(1, 2, 6, 11, 21, 41, 61, 101, 1001, 0),
      max_number_of_regeneration = c(1, 5, 10, 20, 40, 60, 100, 1000, 10000, 0),
      stringsAsFactors = FALSE
    )

  data_regeneration <-
    query_database(database, query_regeneration,
                   selection = selection, conjunction = conjunction) %>%
    mutate(
      year = as.integer(year(.data$date_regeneration)),
      year = ifelse(is.na(.data$year), .data$year_main_survey, .data$year),
      subcircle =
        ifelse(
          .data$height_class %in% c(3000, 4000, 6000, 7000, 8000),
          "A2",
          ifelse(
            .data$height_class %in% c(1000, 2000, 5000, 9000),
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
      subcirclearea_ha =
        ifelse(
          .data$plottype == "CA",
          0.01,
          .data$subcirclearea_ha
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
          .data$plottype == "CP" & is.na(.data$plotarea_ha),
          (pi * .data$r_A2 ^ 2) / 10000,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          is.na(.data$plotarea_ha),
          .data$totalplotarea_ha,
          .data$plotarea_ha
        ),
      rubbing_damage_perc = pmin(
        .data$rubbing_damage_number * 100 / .data$nr_of_regeneration,
        100)
    ) %>%
    left_join(
      number_classes %>%
        select(-"number_class"),
      by = c("number_class" = "id")
    ) %>%
    mutate(
      min_number_of_regeneration =
        ifelse(
          .data$subcircle == "A2" & !is.na(.data$nr_of_regeneration),
          .data$nr_of_regeneration,
          .data$min_number_of_regeneration
        ),
      max_number_of_regeneration =
        ifelse(
          .data$subcircle == "A2" & !is.na(.data$nr_of_regeneration),
          .data$nr_of_regeneration,
          .data$max_number_of_regeneration
        ),
      approx_nr_regeneration =
        ifelse(
          .data$subcircle == "A2" & !is.na(.data$nr_of_regeneration),
          .data$nr_of_regeneration,
          .data$approx_nr_regeneration
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
      approx_nr_regeneration =
        ifelse(
          is.na(.data$approx_nr_regeneration),
          .data$nr_of_regeneration,
          .data$approx_nr_regeneration
        ),
      rubbing_damage_perc =
        ifelse(
          is.na(.data$rubbing_damage_perc),
          .data$rubbing_damage_number * 100 / .data$approx_nr_regeneration,
          .data$rubbing_damage_perc
        )
    ) %>%
    select(-"year_main_survey") %>%
    relocate("approx_nr_regeneration", .after = "nr_of_regeneration") %>%
    relocate(
      ends_with("_number_of_regeneration"), .after = "approx_nr_regeneration"
    )

  attr(data_regeneration, "database") <-
    sub("^.*\\/(.*)\\/.*\\.\\w*$", "\\1", database)
  attr(data_regeneration, "forrescalc") <-
    paste("forrescalc", packageVersion("forrescalc"))

  return(data_regeneration)
}
