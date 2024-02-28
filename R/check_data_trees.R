#' check table trees from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table Trees (of all periods)
#' from the given database and checks for missing data or wrong input.
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#' @param forest_reserve name of forest reserve for which the records in the
#' database should be checked (defaults to "all")
#'
#' @return Dataframe with inconsistent data with ID's and additional columns
#' `aberrant_field` (which column is wrong) and `anomaly` (what is wrong with
#' the input)
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' check_data_trees(path_to_fieldmapdb)
#' check_data_trees(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% anti_join bind_rows count filter group_by left_join
#'   mutate select summarise transmute ungroup
#' @importFrom tidyr pivot_longer
#'
check_data_trees <- function(database, forest_reserve = "all") {
  selection <-
    ifelse(
      forest_reserve == "all", "",
      paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
    )
  query_trees <-
    "SELECT Trees.IDPlots AS plot_id,
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
      Trees.CommonRemark AS commonremark,
      Trees.TreeNumber AS nr_of_stems,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2,
      Trees.OldID
    FROM ((Plots INNER JOIN Trees%2$s Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_%1$deSet pd ON Plots.ID = pd.IDPlots
    %3$s;"

  query_shoots <-
    "SELECT IDPlots AS plot_id,
      XTrees%2$s AS XTrees,
      YTrees%2$s AS YTrees,
      IDTrees%2$s AS IDTrees,
      ID AS shoot_id,
      DBH_mm AS dbh_mm,
      Height_m AS height_m,
      IntactSnag
    FROM Shoots%2$s"

  query_trees_1986 <- sprintf(
    "SELECT Trees.IDPlots AS plot_id,
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
      Trees.CommonRemark AS commonremark,
      Trees.TreeNumber AS nr_of_stems,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2,
      Trees.OldID
    FROM ((Plots INNER JOIN Trees_1986 Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_1986 pd ON Plots.ID = pd.IDPlots
    %1$s;",
    selection
  )

  query_shoots_1986 <-
    "SELECT IDPlots AS plot_id,
      XTrees_1986 AS XTrees,
      YTrees_1986 AS YTrees,
      IDTrees_1986 AS IDTrees,
      ID AS shoot_id,
      DBH_mm AS dbh_mm,
      Height_m AS height_m,
      IntactSnag
    FROM Shoots_1986"

  data_trees <- query_database(database, query_trees, selection = selection)
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
          by = c("plot_id", "X_m" = "XTrees", "Y_m" = "YTrees",
                 "tree_measure_id" = "IDTrees", "period")
        ) %>%
        select(
          "plot_id", "X_m", "Y_m", "tree_measure_id", "period"
        ) %>%
        mutate(
          link_to_layer_shoots = "missing"
        ),
      by = c("plot_id", "X_m", "Y_m", "tree_measure_id", "period")
    ) %>%
    #nr_of_stems (TreeNumber) not correct
    left_join(
      data_trees %>%
        select("plot_id", "tree_measure_id", "nr_of_stems", "period") %>%
        inner_join(
          data_shoots %>%
            count(.data$plot_id, .data$IDTrees, .data$period),
          by = c("plot_id", "tree_measure_id" = "IDTrees", "period")
        ) %>%
        filter(.data$nr_of_stems != .data$n) %>%
        transmute(
          .data$plot_id, .data$tree_measure_id, .data$period,
          field_tree_number = "incorrect"
        ),
      by = c("plot_id", "tree_measure_id", "period")
    ) %>%
    mutate(
      # ratio D/H
      d_h = .data$dbh_mm * pi / (.data$height_m * 10),
      ratio_dbh_height = ifelse(.data$d_h < 1.5, "too low", NA),
      ratio_dbh_height =
        ifelse(.data$d_h > 15, "too high", .data$ratio_dbh_height),
      field_dbh_mm = ifelse(is.na(.data$dbh_mm), "missing", NA),
      field_dbh_mm =
        ifelse(
          !is.na(.data$dbh_mm) & .data$dbh_mm > 2000,
          "too high", .data$field_dbh_mm
        ),
      field_height_m =
        ifelse(is.na(.data$height_m) & .data$intact_snag == 10, "missing", NA),
      field_height_m =
        ifelse(
          !is.na(.data$height_m) & .data$height_m > 50, "too high",
          .data$field_height_m
        ),
      field_height_m =
        ifelse(
          !is.na(.data$height_m) & .data$height_m < 1.3, "too low",
          .data$field_height_m
        ),
      field_species = ifelse(is.na(.data$species), "missing", NA),
      field_intact_snag = ifelse(is.na(.data$intact_snag), "missing", NA),
      field_intact_snag =
        ifelse(
          !.data$intact_snag %in% c(10, 11) & !is.na(.data$intact_snag),
          "not in lookuplist", .data$field_intact_snag
        ),
      field_alive_dead = ifelse(is.na(.data$alive_dead), "missing", NA),
      field_alive_dead =
        ifelse(
          !.data$alive_dead %in% c(11, 12) & !is.na(.data$alive_dead),
          "not in lookuplist", .data$field_alive_dead
        ),
      field_ind_sht_cop = ifelse(is.na(.data$ind_sht_cop), "missing", NA),
      field_ind_sht_cop =
        ifelse(
          !.data$ind_sht_cop %in% c(10, 11, 12) & !is.na(.data$ind_sht_cop),
          "not in lookuplist", .data$field_ind_sht_cop
        ),
      field_ind_sht_cop =
        ifelse(
          .data$ind_sht_cop == 10 & .data$nr_of_stems > 1 &
            !is.na(.data$ind_sht_cop) & !is.na(.data$nr_of_stems),
          "incorrect", .data$field_ind_sht_cop
        ),
      field_ind_sht_cop =
        ifelse(
          .data$ind_sht_cop == 12 & .data$nr_of_stems == 1 &
            !is.na(.data$ind_sht_cop) & !is.na(.data$nr_of_stems),
          "incorrect", .data$field_ind_sht_cop
        ),
      field_decaystage =
        ifelse(
          is.na(.data$decay_stage) & .data$alive_dead == 12, "missing", NA
        ),
      field_decaystage =
        ifelse(
          !is.na(.data$decay_stage) &
            !.data$decay_stage %in% c(10, 11, 12, 13, 14, 15, 16, 17),
          "not in lookuplist",
          .data$field_decaystage),
      field_decaystage =
        ifelse(
          .data$decay_stage %in% c(10, 11, 12, 13, 14, 15, 17) &
            .data$alive_dead == 11 & !is.na(.data$decay_stage),
          "tree alive",
          .data$field_decaystage),
      field_decaystage =
        ifelse(
          (.data$decay_stage == 16 | is.na(.data$decay_stage)) &
            .data$alive_dead == 12,
          "tree not alive",
          .data$field_decaystage),
      field_decaystage =
        ifelse(
          .data$decay_stage == 17 & !is.na(.data$decay_stage) &
            .data$ind_sht_cop == 10,
          "tree no coppice",
          .data$field_decaystage),
      field_iufro_hght = ifelse(is.na(.data$IUFROHght), "missing", NA),
      field_iufro_hght =
        ifelse(
          !is.na(.data$IUFROHght) & !.data$IUFROHght %in% c(10, 20, 30, 40, 50),
          "not in lookuplist", .data$field_iufro_hght
        ),
      field_iufro_hght =
        ifelse(
          .data$IUFROHght %in% c(10, 20, 30, 50) & .data$alive_dead == 12 &
            !is.na(.data$IUFROHght),
          "tree not alive",
          .data$field_iufro_hght),
      field_iufro_hght =
        ifelse(
          .data$IUFROHght == 40 & .data$alive_dead == 11 &
            !is.na(.data$IUFROHght),
          "tree alive", .data$field_iufro_hght
        ),
      field_iufro_hght =
        ifelse(
          .data$IUFROHght == 50 & .data$ind_sht_cop == 10 &
            !is.na(.data$IUFROHght),
          "tree no coppice", .data$field_iufro_hght
        ),
      field_iufro_vital = ifelse(is.na(.data$IUFROVital), "missing", NA),
      field_iufro_vital =
        ifelse(
          !.data$IUFROVital %in% c(10, 20, 30, 40, 50) &
            !is.na(.data$IUFROVital),
          "not in lookuplist", .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$IUFROVital %in% c(10, 20, 30, 50) & .data$alive_dead == 12 &
            !is.na(.data$IUFROVital),
          "tree not alive",
          .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$IUFROVital == 40 & .data$alive_dead == 11 &
            !is.na(.data$IUFROVital),
          "tree alive",
          .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$IUFROVital == 50 & .data$ind_sht_cop == 10 &
            !is.na(.data$IUFROVital),
          "tree no coppice",
          .data$field_iufro_vital
        ),
      field_iufro_socia = ifelse(is.na(.data$IUFROSocia), "missing", NA),
      field_iufro_socia =
        ifelse(
          !.data$IUFROSocia %in% c(10, 20, 30, 40, 50) &
            !is.na(.data$IUFROSocia),
          "not in lookuplist",
          .data$field_iufro_socia),
      field_iufro_socia =
        ifelse(
          .data$IUFROSocia %in% c(10, 20, 30, 50) & .data$alive_dead == 12 &
            !is.na(.data$IUFROSocia),
          "tree not alive",
          .data$field_iufro_socia),
      field_iufro_socia =
        ifelse(
          .data$IUFROSocia == 40 & .data$alive_dead == 11 &
            !is.na(.data$IUFROSocia),
          "tree alive",
          .data$field_iufro_socia
        ),
      field_iufro_socia =
        ifelse(
          .data$IUFROSocia == 50 & .data$ind_sht_cop == 10 &
            !is.na(.data$IUFROSocia),
          "tree no coppice",
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
        ),
      field_common_remark =
        ifelse(
          .data$commonremark == 150 & .data$alive_dead != 11,
          "tree not alive", NA
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
    group_by(.data$plot_id, .data$tree_measure_id, .data$period) %>%
    summarise(
      aberrant_field = paste0(.data$aberrant_field, collapse = " / "),
      anomaly = paste0(.data$anomaly, collapse = " / ")
    ) %>%
    ungroup()

  return(incorrect_trees)
}
