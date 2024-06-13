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
#' @importFrom dplyr %>% anti_join bind_rows count distinct filter left_join
#'   mutate select transmute
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches where
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
      pd.rA3 AS r_a3, pd.rA4 AS r_a4,
      Trees.X_m, Trees.Y_m,
      Trees.ID AS tree_measure_id,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species AS species,
      Trees.IntactSnag AS intact_snag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.CoppiceID AS coppice_id,
      Trees.IUFROHght AS iufro_hght,
      Trees.IUFROVital AS iufro_vital,
      Trees.IUFROSocia AS iufro_socia,
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
    "SELECT shoots.IDPlots AS plot_id,
      shoots.XTrees%2$s AS x_trees,
      shoots.YTrees%2$s AS y_trees,
      shoots.IDTrees%2$s AS id_trees,
      shoots.ID AS shoot_id,
      shoots.DBH_mm AS dbh_mm,
      shoots.Height_m AS height_m,
      shoots.IntactSnag
    FROM Shoots%2$s shoots;"

  query_trees_1986 <- sprintf(
    "SELECT Trees.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      pd.rA3 AS r_a3, pd.rA4 AS r_a4,
      Trees.X_m, Trees.Y_m,
      Trees.ID AS tree_measure_id,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species AS species,
      Trees.IntactSnag AS intact_snag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.CoppiceID AS coppice_id,
      Trees.IUFROHght AS iufro_hght,
      Trees.IUFROVital AS iufro_vital,
      Trees.IUFROSocia AS iufro_socia,
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
    "SELECT shoots.IDPlots AS plot_id,
      shoots.XTrees_1986 AS x_trees,
      shoots.YTrees_1986 AS y_trees,
      shoots.IDTrees_1986 AS id_trees,
      shoots.ID AS shoot_id,
      shoots.DBH_mm AS dbh_mm,
      shoots.Height_m AS height_m,
      shoots.IntactSnag
    FROM Shoots_1986 shoots;"

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
          .data$plottype == "CP" &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > .data$r_a4,
          "tree not in A4",
          NA
        ),
      location =
        ifelse(
          .data$plottype == "CP" & .data$alive_dead == 11 & .data$dbh_mm < 400 &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > .data$r_a3 &
            is.na(.data$location),
          "tree not in A3",
          .data$location
        ),
      location =
        ifelse(
          .data$plottype == "CP" & .data$alive_dead == 12 & .data$dbh_mm < 100 &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > .data$r_a3 &
            is.na(.data$location),
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
          by = c("plot_id", "X_m" = "x_trees", "Y_m" = "y_trees",
                 "tree_measure_id" = "id_trees", "period")
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
            count(.data$plot_id, .data$id_trees, .data$period),
          by = c("plot_id", "tree_measure_id" = "id_trees", "period")
        ) %>%
        filter(.data$nr_of_stems != .data$n) %>%
        transmute(
          .data$plot_id, .data$tree_measure_id, .data$period,
          field_nr_of_stems = "incorrect"
        ),
      by = c("plot_id", "tree_measure_id", "period")
    ) %>%
    mutate(
      # ratio D/H - geen snags
      ratio_dbh_height = round(.data$dbh_mm * pi / (.data$height_m * 10), 1),
      field_ratio_dbh_height =
        ifelse(
          .data$ratio_dbh_height < 1.35 & .data$intact_snag == 11,
          "tree too thin and high", NA),
      field_ratio_dbh_height =
        ifelse(
          .data$ratio_dbh_height > 16.5 & .data$intact_snag == 11,
          "tree too thick and low", .data$field_ratio_dbh_height
        ),
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
          .data$ind_sht_cop %in% c(10, 11) & .data$nr_of_stems > 1 &
            is.na(.data$field_ind_sht_cop) & !is.na(.data$nr_of_stems),
          "incorrect", .data$field_ind_sht_cop
        ),
      field_decay_stage =
        ifelse(
          is.na(.data$decay_stage) & .data$alive_dead == 12 &
            .data$ind_sht_cop %in% c(10, 11),
          "missing", NA
        ),
      field_decay_stage =
        ifelse(
          !is.na(.data$decay_stage) &
            !.data$decay_stage %in% c(10, 11, 12, 13, 14, 15, 16, 17),
          "not in lookuplist",
          .data$field_decay_stage),
      field_decay_stage =
        ifelse(
          .data$decay_stage %in% c(10, 11, 12, 13, 14, 15) &
            .data$alive_dead == 11 & !is.na(.data$decay_stage),
          "tree alive",
          .data$field_decay_stage),
      field_decay_stage =
        ifelse(
          (.data$decay_stage == 16 | is.na(.data$decay_stage)) &
            .data$alive_dead == 12 & is.na(.data$field_decay_stage),
          "tree not alive",
          .data$field_decay_stage),
      field_decay_stage =
        ifelse(
          .data$decay_stage == 17 & !is.na(.data$decay_stage) &
            .data$ind_sht_cop %in% c(10, 11),
          "tree no coppice",
          .data$field_decay_stage),
      field_iufro_hght = ifelse(is.na(.data$iufro_hght) & !period %in% c(0,1),
                                "missing", NA),
      field_iufro_hght =
        ifelse(
          !is.na(.data$iufro_hght) &
            !.data$iufro_hght %in% c(10, 20, 30, 40, 50),
          "not in lookuplist", .data$field_iufro_hght
        ),
      field_iufro_hght =
        ifelse(
          .data$iufro_hght %in% c(10, 20, 30) & .data$alive_dead == 12 &
            !is.na(.data$iufro_hght),
          "tree not alive",
          .data$field_iufro_hght),
      field_iufro_hght =
        ifelse(
          .data$iufro_hght == 40 & .data$alive_dead == 11 &
            !is.na(.data$iufro_hght),
          "tree alive", .data$field_iufro_hght
        ),
      field_iufro_hght =
        ifelse(
          .data$iufro_hght == 50 & .data$ind_sht_cop %in% c(10, 11) &
            !is.na(.data$iufro_hght),
          "tree no coppice", .data$field_iufro_hght
        ),
      field_iufro_vital = ifelse(is.na(.data$iufro_vital) & !period %in% c(0,1),
                                 "missing", NA),
      field_iufro_vital =
        ifelse(
          !.data$iufro_vital %in% c(10, 20, 30, 40, 50) &
            !is.na(.data$iufro_vital),
          "not in lookuplist", .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$iufro_vital %in% c(10, 20, 30) & .data$alive_dead == 12 &
            !is.na(.data$iufro_vital),
          "tree not alive",
          .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$iufro_vital == 40 & .data$alive_dead == 11 &
            !is.na(.data$iufro_vital),
          "tree alive",
          .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$iufro_vital == 50 & .data$ind_sht_cop %in% c(10, 11) &
            !is.na(.data$iufro_vital),
          "tree no coppice",
          .data$field_iufro_vital
        ),
      field_iufro_socia = ifelse(is.na(.data$iufro_socia) & !period %in% c(0,1),
                                 "missing", NA),
      field_iufro_socia =
        ifelse(
          !.data$iufro_socia %in% c(10, 20, 30, 40, 50) &
            !is.na(.data$iufro_socia),
          "not in lookuplist",
          .data$field_iufro_socia),
      field_iufro_socia =
        ifelse(
          .data$iufro_socia %in% c(10, 20, 30) & .data$alive_dead == 12 &
            !is.na(.data$iufro_socia),
          "tree not alive",
          .data$field_iufro_socia),
      field_iufro_socia =
        ifelse(
          .data$iufro_socia == 40 & .data$alive_dead == 11 &
            !is.na(.data$iufro_socia),
          "tree alive",
          .data$field_iufro_socia
        ),
      field_iufro_socia =
        ifelse(
          .data$iufro_socia == 50 & .data$ind_sht_cop %in% c(10, 11) &
            !is.na(.data$iufro_socia),
          "tree no coppice",
          .data$field_iufro_socia),
      field_nr_of_stems =
        ifelse(
          !is.na(.data$nr_of_stems) & .data$nr_of_stems <= 0 &
            is.na(.data$field_nr_of_stems),
          "too low",
          .data$field_nr_of_stems
        ),
      field_coppice_id =
        ifelse(
          !is.na(.data$coppice_id) & .data$ind_sht_cop %in% c(10, 11),
          "unexpected (not missing)",
          NA
        ),
      field_coppice_id =
        ifelse(
          is.na(.data$coppice_id) & .data$ind_sht_cop == 12 &
            !is.na(.data$ind_sht_cop),
          "missing",
          .data$field_coppice_id
        ),
      field_commonremark =
        ifelse(
          .data$commonremark == 150 & .data$alive_dead != 11,
          "tree not alive", NA
        ),
      tree_measure_id = as.character(.data$tree_measure_id),
      species = as.character(.data$species)
    ) %>%
    bind_rows(
      data_trees %>%
        filter(!is.na(.data$coppice_id)) %>%
        group_by(
          .data$plot_id, .data$period, .data$coppice_id, .data$alive_dead
        ) %>%
        mutate(
          n_records = n(),
          tree_measure_id_diff = paste(.data$tree_measure_id, collapse = "_")
        ) %>%
        ungroup() %>%
        filter(.data$n_records > 1) %>%
        transmute(
          .data$plot_id, .data$period, .data$coppice_id, .data$alive_dead,
          tree_measure_id = .data$tree_measure_id_diff,
          field_coppice_id =
            paste0(.data$n_records, " times the same coppice_id")
        ) %>%
        distinct()
    ) %>%
    bind_rows(
      data_trees %>%
        filter(!is.na(.data$coppice_id)) %>%
        group_by(.data$plot_id, .data$period, .data$coppice_id) %>%
        mutate(
          n_records = n(),
          species_diff = max(.data$species) - min(.data$species),
          x_m_diff = max(.data$X_m) - min(.data$X_m),
          y_m_diff = max(.data$Y_m) - min(.data$Y_m),
          location_shift = sqrt(.data$x_m_diff ^ 2 + .data$y_m_diff ^ 2),
          tree_measure_id_diff = paste(.data$tree_measure_id, collapse = "_"),
          species = paste(.data$species, collapse = "_")
        ) %>%
        ungroup() %>%
        filter(.data$n_records > 1) %>%
        transmute(
          .data$plot_id, .data$period, .data$coppice_id, .data$species,
          location_shift = round(.data$location_shift, 2),
          tree_measure_id = .data$tree_measure_id_diff,
          field_species =
            ifelse(.data$species_diff == 0, NA, "shifter in coppice tree"),
          field_location_shift =
            ifelse(.data$location_shift > 1, "walker in coppice tree", NA)
        ) %>%
        filter(
          !(is.na(.data$field_species) & is.na(.data$field_location_shift))
        ) %>%
        distinct()
    ) %>%
    mutate(
      across(
        where(~ is.numeric(.x)) & !matches(c("plot_id", "period")),
        as.character
      )
    ) %>%
    pivot_longer(
      cols =
        c("location", "link_to_layer_shoots", starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      plottype = NULL, remark = NULL
    ) %>%
    pivot_longer(
      cols =
        !c("plot_id", "tree_measure_id", "period", "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(
      .data$aberrant_field == .data$varname |
        .data$aberrant_field %in%
          c("location", "link_to_layer_shoots")
    ) %>%
    mutate(
      aberrant_value =
        ifelse(
          .data$aberrant_field %in%
            c("location", "link_to_layer_shoots"),
          NA,
          .data$aberrant_value
        )
    ) %>%
    select(-"varname") %>%
    distinct()

  return(incorrect_trees)
}
