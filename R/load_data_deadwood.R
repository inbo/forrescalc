#' retrieves data on logs from fieldmap database
#'
#' This function queries the given database to retrieve data on deadwood (logs)
#' (ready for use in calculate_dendrometry function).
#'
#' @param extra_variables Should additional variables such as
#' remark and common_remark be added?
#' Default is FALSE (no).
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with data on logs
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' load_data_deadwood(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_replace
#' @importFrom lubridate month year
#' @importFrom utils packageVersion
#'
load_data_deadwood <-
  function(database, plottype = NA, forest_reserve = NA,
           extra_variables = FALSE, processed = TRUE) {
    selection <-
      translate_input_to_selectquery(
        database = database, plottype = plottype,
        forest_reserve = forest_reserve, processed = processed,
        survey_name = "Survey_Deadwood_YN"
      )
    selection <-
      ifelse(
        selection == "", selection,
        str_replace(selection, "WHERE", "AND")
      )
    add_fields <-
      ifelse(
        extra_variables,
        ", Deadwood.Remark AS remark, Deadwood.CommonRemark AS common_remark",
        ""
      )
  query_deadwood <-
    "SELECT Plots.ID AS plot_id,
      qPlotType.Value3 AS plottype,
      IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha)
        AS totalplotarea_ha,
      pd.ForestReserve AS forest_reserve,
      pd.Date_Dendro_%1$deSet AS date_dendro,
      pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
      pd.LengthCoreArea_m AS length_core_area_m,
      pd.WidthCoreArea_m AS width_core_area_m,
      pd.Area_ha AS core_area_ha,
      Deadwood.ID AS lying_deadw_id,
      Deadwood.Species AS species,
      Deadwood.DecayStage AS decaystage,
      Deadwood.IntactFragment AS intact_fragm,
      Deadwood.CalcVolume_m3 AS calc_volume_m3,
      Deadwood.CalcLength_m AS calc_length_m,
      Deadw_Diam.total_length_m,
      Deadw_Diam.min_diam_mm,
      Deadw_Diam.max_diam_mm %4$s
    FROM ((((Plots
      INNER JOIN Deadwood%2$s Deadwood ON Plots.ID = Deadwood.IDPlots)
      INNER JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      LEFT JOIN
        (SELECT IDDeadwood%2$s, IDPlots,
          MAX(Distance_m) AS total_length_m,
          MIN(Diameter_mm) AS min_diam_mm,
          MAX(Diameter_mm) AS max_diam_mm
        FROM Deadwood%2$s_Diameters
        GROUP BY IDDeadwood%2$s, IDPlots) Deadw_Diam
        ON Deadwood.ID = Deadw_Diam.IDDeadwood%2$s)
      WHERE Plots.ID = Deadw_Diam.IDPlots %3$s;"

  data_deadwood <-
    query_database(database, query_deadwood,
                   selection = selection, add_fields = add_fields) %>%
    mutate(
      year =
        as.integer(year(.data$date_dendro) - (month(.data$date_dendro) < 5)),
      dbh_class_5cm = give_diamclass_5cm(.data$max_diam_mm),
      plotarea_ha =
        ifelse(
          .data$plottype == "CP",
          (pi * .data$r_A4 ^ 2) / 10000,
          NA
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == "CA",
          .data$length_core_area_m * .data$width_core_area_m / 10000,
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
        )
    )

  attr(data_deadwood, "database") <- file_path_sans_ext(basename(database))
  attr(data_deadwood, "forrescalc") <-
    paste("forrescalc", packageVersion("forrescalc"))

  return(data_deadwood)
}
