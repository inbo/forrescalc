#' check table trees from fieldmap database for inconsistencies
#'
#' STILL UNDER CONSTRUCTION!!!
#' This function retrieves the important fields of table Trees (and Trees_2eSET)
#' from the given database and checks for missing data or wrong input.
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#'
#' @return Dataframe with inconsistent data
#' (FOR NOW ALL DATA WITH EXTRA COLUMN 'PROBLEM')
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' check_data_trees(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% anti_join bind_rows filter group_by left_join mutate
#'   select summarise ungroup
#' @importFrom tidyr pivot_longer
#'
check_data_trees <- function(database) {
  query_trees <-
    "SELECT Trees.IDPlots,
      qPlotType.Value3 AS plottype,
      Trees.X_m, Trees.Y_m,
      Trees.ID AS tree_measure_id,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species AS species,
      Trees.IntactSnag AS intact_snag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.CoppiceID AS coppice_id,
      Trees.IUFROHght, Trees.IUFROVital, Trees.IUFROSocia,
      Trees.DecayStage AS decay_stage,
      Trees.Remark AS remark,
      Trees.TreeNumber AS nr_of_stems,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2,
      Trees.OldID
    FROM (Plots INNER JOIN Trees%2$s Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_shoots <-
    "SELECT IDPlots,
      XTrees%2$s AS XTrees,
      YTrees%2$s AS YTrees,
      IDTrees%2$s AS IDTrees,
      ID AS shoot_id,
      DBH_mm AS dbh_mm,
      Height_m AS height_m,
      IntactSnag
    FROM Shoots%2$s"

  query_trees_1986 <-
    "SELECT Trees.IDPlots,
      qPlotType.Value3 AS plottype,
      Trees.X_m, Trees.Y_m,
      Trees.ID AS tree_measure_id,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species AS species,
      Trees.IntactSnag AS intact_snag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.CoppiceID AS coppice_id,
      Trees.IUFROHght, Trees.IUFROVital, Trees.IUFROSocia,
      Trees.DecayStage AS decay_stage,
      Trees.Remark AS remark,
      Trees.TreeNumber AS nr_of_stems,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2,
      Trees.OldID
    FROM (Plots INNER JOIN Trees_1986 Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_shoots_1986 <-
    "SELECT IDPlots,
      XTrees_1986 AS XTrees,
      YTrees_1986 AS YTrees,
      IDTrees_1986 AS IDTrees,
      ID AS shoot_id,
      DBH_mm AS dbh_mm,
      Height_m AS height_m,
      IntactSnag
    FROM Shoots_1986"

  data_trees <- query_database(database, query_trees)
  data_shoots <- query_database(database, query_shoots)

  con <- connect_to_database(database)
  data_trees_1986 <- dbGetQuery(con, query_trees_1986) %>%
    mutate(period = 0)
  if (nrow(data_trees_1986) > 0) {
    data_trees <- data_trees %>%
      bind_rows(
        data_trees_1986
      )
  }
  data_shoots_1986 <- dbGetQuery(con, query_shoots_1986) %>%
    mutate(period = 0)
  if (nrow(data_shoots_1986) > 0) {
    data_shoots <- data_shoots %>%
      bind_rows(
        data_shoots_1986
      )
  }
  dbDisconnect(con)

  incorrect_trees <- data_trees %>%
    # not in A3 or A4
    mutate(
      location =
        ifelse(
          .data$plottype == "CP" & sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > 18,
          "tree not in A4",
          NA
        ),
      location =
        ifelse(
          .data$plottype == "CP" & .data$alive_dead == 11 & .data$dbh_mm < 400 &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > 9 & is.na(.data$location),
          "tree not in A3",
          .data$location
        ),
      location =
        ifelse(
          .data$plottype == "CP" & .data$alive_dead == 12 & .data$dbh_mm < 100 &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > 9 & is.na(.data$location),
          "tree not in A3",
          .data$location
        )
    ) %>%
    # shoots not correctly linked with trees
    left_join(
      data_trees %>%
        filter(.data$ind_sht_cop == 12) %>%
        anti_join(
          data_shoots,
          by = c("IDPlots", "X_m" = "XTrees", "Y_m" = "YTrees",
                 "tree_measure_id" = "IDTrees", "period")
        ) %>%
        select(
          "IDPlots", "X_m", "Y_m", "tree_measure_id", "period"
        ) %>%
        mutate(
          link_to_layer_shoots = "missing"
        ),
      by = c("IDPlots", "X_m", "Y_m", "tree_measure_id", "period")
    ) %>%
    mutate(
      d_h = .data$dbh_mm * pi / (.data$height_m * 10),
      ratio_dbh_height = ifelse(.data$d_h < 1.5, "too low", NA),
      ratio_dbh_height =
        ifelse(.data$d_h > 15, "too high", .data$ratio_dbh_height),
      field_dbh_mm = ifelse(is.na(.data$dbh_mm), "missing", NA),
      field_dbh_mm =
        ifelse(.data$dbh_mm > 2000, "too high", .data$field_dbh_mm),
      field_height_m =
        ifelse(is.na(.data$height_m) & .data$intact_snag == 10, "missing", NA),
      field_height_m =
        ifelse(.data$height_m > 50, "too high", .data$field_height_m),
      field_height_m =
        ifelse(.data$height_m < 1.3, "too low", .data$field_height_m),
      field_species = ifelse(is.na(.data$species), "missing", NA),
      field_intact_snag = ifelse(is.na(.data$intact_snag), "missing", NA),
      field_intact_snag =
        ifelse(!.data$intact_snag %in% c(10, 11), "not in lookuplist",
               .data$field_intact_snag),
      field_alive_dead = ifelse(is.na(.data$alive_dead), "missing", NA),
      field_alive_dead =
        ifelse(!.data$alive_dead %in% c(11, 12), "not in lookuplist",
               .data$field_alive_dead),
      field_ind_sht_cop = ifelse(is.na(.data$ind_sht_cop), "missing", NA),
      field_ind_sht_cop =
        ifelse(!.data$ind_sht_cop %in% c(10, 11, 12), "not in lookuplist",
               .data$field_ind_sht_cop),
      field_decaystage = ifelse(is.na(.data$decay_stage), "missing", NA),
      field_decaystage =
        ifelse(
          !.data$decay_stage %in% c(10, 11, 12, 13, 14, 15, 16),
          "not in lookuplist",
          .data$field_decaystage),
      field_decaystage =
        ifelse(
          .data$decay_stage %in% c(10, 11, 12, 13, 14, 15) &
            .data$alive_dead == 11,
          "tree not dead",
          .data$field_decaystage),
      field_decaystage =
        ifelse(
          .data$decay_stage == 16 & .data$alive_dead == 12,
          "tree not alive",
          .data$field_decaystage),
      field_iufro_hght = ifelse(is.na(.data$IUFROHght), "missing", NA),
      field_iufro_hght =
        ifelse(!.data$IUFROHght %in% c(10, 20, 30, 40), "not in lookuplist",
               .data$field_iufro_hght),
      field_iufro_hght =
        ifelse(
          .data$IUFROHght %in% c(10, 20, 30) & .data$alive_dead == 12,
          "tree not alive",
          .data$field_iufro_hght),
      field_iufro_hght =
        ifelse(.data$IUFROHght == 40 & .data$alive_dead == 11, "tree not dead",
               .data$field_iufro_hght),
      field_iufro_vital = ifelse(is.na(.data$IUFROVital), "missing", NA),
      field_iufro_vital =
        ifelse(!.data$IUFROVital %in% c(10, 20, 30, 40), "not in lookuplist",
               .data$field_iufro_vital),
      field_iufro_vital =
        ifelse(
          .data$IUFROVital %in% c(10, 20, 30) & .data$alive_dead == 12,
          "tree not alive",
          .data$field_iufro_vital),
      field_iufro_vital =
        ifelse(.data$IUFROVital == 40 & .data$alive_dead == 11, "tree not dead",
               .data$field_iufro_vital),
      field_iufro_socia = ifelse(is.na(.data$IUFROSocia), "missing", NA),
      field_iufro_socia =
        ifelse(!.data$IUFROSocia %in% c(10, 20, 30, 40), "not in lookuplist",
               .data$field_iufro_socia),
      field_iufro_socia =
        ifelse(
          .data$IUFROSocia %in% c(10, 20, 30) & .data$alive_dead == 12,
          "tree not alive",
          .data$field_iufro_socia),
      field_iufro_socia =
        ifelse(.data$IUFROSocia == 40 & .data$alive_dead == 11, "tree not dead",
               .data$field_iufro_socia),
      field_treenumber =
        ifelse(!is.na(.data$nr_of_stems) & .data$nr_of_stems <= 0, "too low",
               NA),
      field_coppice_id =
        ifelse(
          !is.na(.data$coppice_id) & .data$ind_sht_cop == 10,
          "unexpected (not missing)",
          NA
        ),
      field_coppice_id =
        ifelse(
          is.na(.data$coppice_id) & .data$ind_sht_cop != 10,
          "missing",
          .data$field_coppice_id
        )
    ) %>%
    pivot_longer(
      cols =
        c("location", "link_to_layer_shoots", "ratio_dbh_height",
          starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field)
    ) %>%
    group_by(.data$IDPlots, .data$tree_measure_id, .data$period) %>%
    summarise(
      aberrant_field = paste0(.data$aberrant_field, collapse = " / "),
      anomaly = paste0(.data$anomaly, collapse = " / ")
    ) %>%
    ungroup()

  return(incorrect_trees)
}
