#' retrieve dendrometry data from fieldmap database
#'
#' This function queries the given database to retrieve data on dendrometry
#' (ready for use in calculate_dendrometry function).
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#' @param plottype possibility to select only data for a certain plot type, e.g. 'Circular plot'
#' or 'Core area' (the default NA means that data from all plots are retrieved)
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
#' library(forrescalc)
#' load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
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
        (Trees.X_m - Plots.Xorig_m) AS x_local, (Trees.Y_m - Plots.Yorig_m) AS y_local,
        Trees.CrownVolumeReduction AS crown_volume_reduction,
        Trees.BranchLengthReduction AS branch_length_reduction,
        Trees.CoppiceID AS coppice_id, Trees.IUFROHght AS iufro_hght,
        Trees.IUFROVital AS iufro_vital, IUFROSocia AS iufro_socia,
        Trees.Remark AS remark, Trees.CommonRemark AS common_remark",
      ""
    )
  query_dendro <-
      "SELECT Plots.ID AS plot_id,
        Plots.Plottype AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
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
        Trees.DecayStage AS decaystage,
        Trees.Vol_tot_m3 AS vol_tot_m3,
        Trees.Vol_stem_m3 AS vol_stem_m3,
        Trees.Vol_crown_m3 AS vol_crown_m3,
        Trees.BasalArea_m2 AS basal_area_m2,
        Trees.IndShtCop AS ind_sht_cop,
        Trees.TreeNumber AS tree_number,
        Trees.Individual AS individual %4$s
      FROM ((Plots INNER JOIN Trees%2$s Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots) %3$s;"

  data_dendro <-
    query_database(database, query_dendro,
                   selection = selection, add_fields = add_fields) %>%
    mutate(
      year = year(round_date(.data$date_dendro, "year")) - 1,
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
          (pi * .data$r_A4 ^ 2)/10000,
          (pi * .data$r_A3 ^ 2)/10000
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == 20,
          .data$subcirclearea_ha,
          NA
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == 30,
          (.data$length_core_area_m * .data$width_core_area_m)/10000,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == 30 & is.na(.data$plotarea_ha),
          .data$core_area_ha,
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
          .data$alive_dead == 11,
          .data$basal_area_m2 / .data$plotarea_ha,
          0
        ),
      basal_area_snag_m2_ha =
        ifelse(
          .data$alive_dead == 12,
          .data$basal_area_m2 / .data$plotarea_ha,
          0
        ),
      volume_alive_m3_ha =
        ifelse(
          .data$alive_dead == 11,
          .data$vol_tot_m3 / .data$plotarea_ha,
          0
        ),
      volume_snag_m3_ha =
        ifelse(
          .data$alive_dead == 12,
          .data$vol_tot_m3 / .data$plotarea_ha,
          0
        ),
      volume_stem_alive_m3_ha =
        ifelse(
          .data$alive_dead == 11,
          .data$vol_stem_m3 / .data$plotarea_ha,
          0
        ),
      volume_stem_snag_m3_ha =
        ifelse(
          .data$alive_dead == 12,
          .data$vol_stem_m3 / .data$plotarea_ha,
          0
        ),
      dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm)
    )

  return(data_dendro)
}
