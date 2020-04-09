#' retrieve dendrometry data from fieldmap database
#'
#' This function queries the given database to retrieve data on dendrometry
#' (ready for use in calculate_dendrometry function).
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#' @param plottype possibility to select only data for a certain plot type, e.g. 'circular plot'
#' or 'core area' (the default NA means that data from all plots are retrieved)
#' @param forest_reserve possibility to select only data for 1 forest reserve
#' by giving the name of the forest reserve (the default NA means that data
#' from all plots are retrieved)
#' @param extra_variables Should additional variables such as calc_height_m,
#' intact_snag, x_m, y_m, crown_volume_reduction, branche_length_reduction,
#' coppice_id, iufro_hght, iufro_vital, iufro_socia, remark and common_remark be added?
#' Default is FALSE (no).
#'
#' @return Dataframe with dendrometry data
#'
#' @examples
#' \dontrun{
#' #change path before running
#' load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows mutate
#' @importFrom lubridate round_date year
#'
load_data_dendrometry <-
  function(database, plottype = NA, forest_reserve = NA, extra_variables = FALSE) {
  selection <-
    translate_input_to_selectionquery(database, plottype, forest_reserve)
  add_fields <-
    ifelse(
      extra_variables,
      ", Trees.Calcheight_m AS calc_height_m, Trees.IntactSnag AS intact_snag,
        (Trees.X_m - Plots.Xorig_m) AS x_m, (Trees.Y_m - Plots.Yorig_m) AS y_m,
        Trees.CrownVolumeReduction AS crown_volume_reduction,
        Trees.BrancheLengthReduction AS branche_length_reduction,
        Trees.CoppiceID AS coppice_id, Trees.IUFROHght AS iufro_hght,
        Trees.IUFROVital AS iufro_vital, IUFROSocia AS iufro_socia,
        Trees.Remark AS remark, Trees.CommenRemark AS common_remark",
      ""
    )
  query_dendro <-
    sprintf(
      "SELECT Plots.ID AS plot_id,
        Plots.Plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
        pd.ForestReserve,
        pd.Date_dendro_1eSet AS date_dendro,
        pd.rA1, pd.rA2, pd.rA3, pd.rA4,
        pd.LenghtCoreArea_m, pd.WidthCoreArea_m,
        Trees.DBH_mm,
        Trees.Height_m,
        Trees.Species AS species,
        Trees.AliveDead,
        Trees.DecayStage AS decaystage,
        Trees.Vol_tot_m3,
        Trees.Vol_stem_m3,
        Trees.Vol_crown_m3,
        Trees.BasalArea_m2,
        Trees.IndShtCop,
        Trees.TreeNumber,
        Trees.Individual %s
      FROM ((Plots INNER JOIN Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots) %s;",
      add_fields, selection
    )

  query_dendro2 <-
    sprintf(
      "SELECT Plots.ID AS plot_id,
        Plots.Plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
        pd.ForestReserve,
        pd.Date_dendro_2eSet AS date_dendro,
        pd.rA1, pd.rA2, pd.rA3, pd.rA4,
        pd.LenghtCoreArea_m, pd.WidthCoreArea_m,
        Trees.DBH_mm,
        Trees.Height_m,
        Trees.Species AS species,
        Trees.AliveDead,
        Trees.DecayStage AS decaystage,
        Trees.Vol_tot_m3,
        Trees.Vol_stem_m3,
        Trees.Vol_crown_m3,
        Trees.BasalArea_m2,
        Trees.IndShtCop,
        Trees.TreeNumber,
        Trees.Individual %s,
        Trees.OldID as old_id
      FROM ((Plots INNER JOIN Trees_2eSET Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots) %s;",
      add_fields, selection
    )

  con <- odbcConnectAccess2007(database)
  data_dendro <- sqlQuery(con, query_dendro, stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_dendro2, stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    ) %>%
    mutate(
      year = year(round_date(.data$date_dendro, "year")) - 1,
      subcircle =
        ifelse(
          .data$AliveDead == 11 & .data$DBH_mm >= 400,
          "A4",
          ifelse(
            .data$AliveDead == 12 & .data$DBH_mm >= 100,
            "A4",
            "A3"
          )
        ),
      subcirclearea_ha =
        ifelse(
          .data$subcircle == "A4",
          (pi * .data$rA4 ^ 2)/10000,
          (pi * .data$rA3 ^ 2)/10000
        ),
      plotarea_ha =
        ifelse(
          .data$Plottype == 20,
          .data$subcirclearea_ha,
          NA
        ),
      plotarea_ha =
        ifelse(
          .data$Plottype == 30,
          .data$LenghtCoreArea_m * .data$WidthCoreArea_m,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          is.na(.data$plotarea_ha),
          .data$totalplotarea_ha,
          .data$plotarea_ha
        ),
      basal_area_alive_m2_ha =
        ifelse(
          .data$AliveDead == 11,
          .data$BasalArea_m2 / .data$plotarea_ha,
          0
        ),
      basal_area_snag_m2_ha =
        ifelse(
          .data$AliveDead == 12,
          .data$BasalArea_m2 / .data$plotarea_ha,
          0
        ),
      volume_alive_m3_ha =
        ifelse(
          .data$AliveDead == 11,
          .data$Vol_tot_m3 / .data$plotarea_ha,
          0
        ),
      volume_snag_m3_ha =
        ifelse(
          .data$AliveDead == 12,
          .data$Vol_tot_m3 / .data$plotarea_ha,
          0
        ),
      DBHClass_5cm = give_diamclass_5cm(.data$DBH_mm)
    )
  odbcClose(con)

  return(data_dendro)
}
