#' check table trees from fieldmap database for inconsistencies
#'
#' STILL UNDER CONSTRUCTION!!!
#' This function retrieves the important fields of table Trees (and Trees_2eSET) from the given database and checks for lacking data or wrong input.
#'
#' @param database name of fieldmap/access database (with specific fieldmap structure) including path
#'
#' @return Dataframe with inconsistent data (FOR NOW ALL DATA WITH EXTRA COLUMN 'PROBLEM')
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' check_data_trees("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% anti_join bind_rows filter full_join left_join mutate select
#'
check_data_trees <- function(database) {
  query_trees <-
    "SELECT Trees.IDPlots,
      qPlotType.Value3 AS plottype,
      Trees.X_m, Trees.Y_m,
      Trees.ID AS tree_measure_id,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species,
      Trees.IntactSnag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.CoppiceID,
      Trees.IUFROHght, Trees.IUFROVital, Trees.IUFROSocia,
      Trees.DecayStage,
      Trees.Remark,
      Trees.TreeNumber AS tree_number,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2
    FROM (Plots INNER JOIN Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_shoots <-
    "SELECT IDPlots, XTrees, YTrees, IDTrees, ID AS shoot_id, DBH_mm AS dbh_mm, Height_m AS height_m, IntactSnag
    FROM Shoots"

  query_trees2 <-
    "SELECT Trees.IDPlots,
      qPlotType.Value3 AS plottype,
      Trees.X_m, Trees.Y_m,
      Trees.ID AS tree_measure_id,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species,
      Trees.IntactSnag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.CoppiceID,
      Trees.IUFROHght, Trees.IUFROVital, Trees.IUFROSocia,
      Trees.DecayStage,
      Trees.Remark,
      Trees.TreeNumber AS tree_number,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2,
      Trees.OldID
    FROM (Plots INNER JOIN Trees_2eSET Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_shoots2 <-
    "SELECT IDPlots,
      XTrees_2eSET AS XTrees, YTrees_2eSET AS YTrees, IDTrees_2eSET AS IDTrees, ID AS shoot_id,
      DBH_mm AS dbh_mm, Height_m AS height_m, IntactSnag
    FROM Shoots_2eSET"

  con <- odbcConnectAccess2007(database)
  data_trees <- sqlQuery(con, query_trees, stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_trees2, stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    )
  data_shoots <- sqlQuery(con, query_shoots, stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_shoots2, stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    )
  odbcClose(con)

  incorrect_trees <- data_trees %>%
    # niet in A3 of A4
    mutate(
      problem =
        ifelse(
          .data$plottype == "CP" & sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > 18,
          "tree not in A4",
          NA
        ),
      problem =
        ifelse(
          .data$plottype == "CP" & .data$alive_dead == 11 & .data$dbh_mm < 400 &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > 9,
          "tree not in A3",
          .data$problem
        ),
      problem =
        ifelse(
          .data$plottype == "CP" & .data$alive_dead == 12 & .data$dbh_mm < 100 &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > 9,
          "tree not in A3",
          .data$problem
        )
    ) %>%
    # shoots niet correct gelinkt met trees
    left_join(
      data_trees %>%
        filter(.data$ind_sht_cop == 12) %>%
        anti_join(
          data_shoots,
          by = c("IDPlots", "X_m" = "XTrees", "Y_m" = "YTrees",
                 "tree_measure_id" = "IDTrees", "period")
        ) %>%
        select(
          .data$IDPlots, .data$X_m, .data$Y_m, .data$tree_measure_id,
          .data$period
        ) %>%
        mutate(
          coppiceproblem = "no shoots linked with coppice tree"
        ),
      by = c("IDPlots", "X_m", "Y_m", "tree_measure_id", "period")
    ) %>%
    mutate(
      problem =
        ifelse(
          !is.na(.data$problem),
          ifelse(
            !is.na(.data$coppiceproblem),
            paste(.data$problem, .data$coppiceproblem, sep = " / "),
            .data$problem
          ),
          .data$coppiceproblem
        ),
      coppiceproblem = NULL
    ) %>%
    full_join(
       data_shoots %>%
        anti_join(
          data_trees %>%
            filter(.data$ind_sht_cop == 12),
          by = c("IDPlots", "XTrees" = "X_m", "YTrees" = "Y_m",
                 "IDTrees" = "tree_measure_id", "period")
        ) %>%
        select(
          .data$IDPlots, X_m = .data$XTrees, Y_m = .data$YTrees,
          tree_measure_id = .data$IDTrees, .data$period
        ) %>%
        mutate(
          coppiceproblem = "no coppice tree linked with shoots"
        ),
      by = c("IDPlots", "X_m", "Y_m", "tree_measure_id", "period")
    ) %>%
    mutate(
      problem =
        ifelse(
          !is.na(.data$problem),
          ifelse(
            !is.na(.data$coppiceproblem),
            paste(.data$problem, .data$coppiceproblem, sep = " / "),
            .data$problem
          ),
          .data$coppiceproblem
        ),
      coppiceproblem = NULL
    ) %>%
    filter(!is.na(.data$problem))

  return(incorrect_trees)
}
