#' retrieve dendrometry data from fieldmap database
#'
#' This function queries the given database to retrieve data on dendrometry
#' (ready for use in calculate_dendrometry function).
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#' @param plottype possibility to select only data for a certain plot type, e.g.
#' 'CP' for Circular plot or 'CA' for Core area
#' (the default NA means that data from all plots are retrieved)
#' @param forest_reserve possibility to select only data for 1 forest reserve
#' by giving the name of the forest reserve (the default NA means that data
#' from all plots are retrieved)
#' @param extra_variables Should additional variables such as
#' x_m, y_m, coppice_id, iufro_hght, iufro_vital, iufro_socia,
#' remark and common_remark be added?
#' Default is FALSE (no).
#' @param processed Should only processed and surveyed data be added?
#' Defaults to TRUE (yes).
#'
#' @return Dataframe with dendrometry data
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' load_data_dendrometry(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
#' @importFrom lubridate month year
#' @importFrom DBI dbDisconnect dbGetQuery
#'
load_data_dendrometry <-
  function(database, plottype = NA, forest_reserve = NA,
           extra_variables = FALSE, processed = TRUE) {
  selection <-
    translate_input_to_selectionquery(
      database = database, plottype = plottype, forest_reserve = forest_reserve,
      processed = processed, survey_name = "Survey_Trees_YN"
    )
  add_fields <-
    ifelse(
      extra_variables,
      ", (Trees.X_m - Plots.Xorig_m) AS x_local, (Trees.Y_m - Plots.Yorig_m) AS y_local,
        Trees.CoppiceID AS coppice_id, Trees.IUFROHght AS iufro_hght,
        Trees.IUFROVital AS iufro_vital, Trees.IUFROSocia AS iufro_socia,
        Trees.Remark AS remark, Trees.CommonRemark AS common_remark",
      ""
    )
  query_dendro <-
      "SELECT Plots.ID AS plot_id,
        qPlotType.Value3 AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
        Trees.OldID AS old_id,
        pd.ForestReserve AS forest_reserve,
        pd.Date_Dendro_%1$deSet AS date_dendro,
        pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
        pd.TresHoldDBH_Trees_A3_alive AS dbh_min_a3,
        pd.TresHoldDBH_Trees_A3_dead AS dbh_min_a3_dead,
        pd.TresHoldDBH_Trees_A4_alive AS dbh_min_a4,
        pd.TresHoldDBH_Trees_A4_dead AS dbh_min_a4_dead,
        pd.TresHoldDBH_Trees_CoreArea_alive AS dbh_min_core_area,
        pd.TresHoldDBH_Trees_CoreArea_dead AS dbh_min_core_area_dead,
        pd.LengthCoreArea_m AS length_core_area_m,
        pd.WidthCoreArea_m AS width_core_area_m,
        pd.Area_ha AS core_area_ha,
        Trees.DBH_mm AS dbh_mm,
        Trees.Height_m AS height_m,
        Trees.Species AS species,
        Trees.AliveDead AS alive_dead,
        Trees.IntactSnag AS intact_snag,
        Trees.DecayStage AS decaystage,
        Trees.Calcheight_m AS calc_height_fm,
        cvr.Value3 AS crown_volume_reduction,
        blr.Value3 AS branch_length_reduction,
        Trees.IndShtCop AS ind_sht_cop,
        Trees.TreeNumber AS nr_of_stems %4$s
      FROM (((((Plots INNER JOIN Trees%2$s Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots)
        INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
        LEFT JOIN qCrownVolRedu cvr ON Trees.CrownVolumeReduction = cvr.ID)
        LEFT JOIN qBranchLenghtReduction blr ON Trees.BranchLengthReduction = blr.ID) %3$s;"

  query_dendro_1986 <-
    sprintf(
      "SELECT Plots.ID AS plot_id,
        qPlotType.Value3 AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
        Trees.OldID AS old_id,
        pd.ForestReserve AS forest_reserve,
        pd.Date_Dendro_1986 AS date_dendro,
        pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
        pd.TresHoldDBH_Trees_A3_alive AS dbh_min_a3,
        pd.TresHoldDBH_Trees_A3_dead AS dbh_min_a3_dead,
        pd.TresHoldDBH_Trees_A4_alive AS dbh_min_a4,
        pd.TresHoldDBH_Trees_A4_dead AS dbh_min_a4_dead,
        pd.TresHoldDBH_Trees_CoreArea_alive AS dbh_min_core_area,
        pd.TresHoldDBH_Trees_CoreArea_dead AS dbh_min_core_area_dead,
        pd.LengthCoreArea_m AS length_core_area_m,
        pd.WidthCoreArea_m AS width_core_area_m,
        pd.Area_ha AS core_area_ha,
        Trees.DBH_mm AS dbh_mm,
        Trees.Height_m AS height_m,
        Trees.Species AS species,
        Trees.AliveDead AS alive_dead,
        Trees.IntactSnag AS intact_snag,
        Trees.DecayStage AS decaystage,
        Trees.Calcheight_m AS calc_height_fm,
        cvr.Value3 AS crown_volume_reduction,
        blr.Value3 AS branch_length_reduction,
        Trees.IndShtCop AS ind_sht_cop,
        Trees.TreeNumber AS nr_of_stems %2$s
      FROM (((((Plots INNER JOIN Trees_1986 Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN PlotDetails_1986 pd ON Plots.ID = pd.IDPlots)
        INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
        LEFT JOIN qCrownVolRedu cvr ON Trees.CrownVolumeReduction = cvr.ID)
        LEFT JOIN qBranchLenghtReduction blr ON Trees.BranchLengthReduction = blr.ID) %1$s;",
      selection, add_fields
    )

  con <- connect_to_database(database)
  dendro_1986 <- dbGetQuery(con, query_dendro_1986) %>%
    mutate(period = 0)
  if (class(con) == "SQLiteConnection") {
    dendro_1986 <- dendro_1986 %>%
      mutate(
        date_dendro = as.POSIXct(.data$date_dendro, origin = "1970-01-01")
      )
  }
  dbDisconnect(con)

  data_dendro <-
    query_database(database, query_dendro,
                   selection = selection, add_fields = add_fields)
  if (nrow(dendro_1986) > 0) {
    data_dendro <- data_dendro %>%
      bind_rows(
        dendro_1986
      )
  }
  data_dendro <- data_dendro %>%
    mutate(
      year = year(.data$date_dendro) - (month(.data$date_dendro) < 5 ),
      subcircle =
        ifelse(
          .data$alive_dead == 11 & .data$dbh_mm >= .data$dbh_min_a4,
          "A4",
          ifelse(
            .data$alive_dead == 12 & .data$dbh_mm >= .data$dbh_min_a4_dead,
            "A4",
            "A3"
          )
        ),
      subcirclearea_ha =
        ifelse(
          .data$subcircle == "A4",
          (pi * .data$r_A4 ^ 2) / 10000,
          (pi * .data$r_A3 ^ 2) / 10000
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
      dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm)
    )

  return(data_dendro)
}
