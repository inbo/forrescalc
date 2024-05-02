context("test calculate_dendrometry basics")

path_to_fieldmapdb <-
  system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")

data_dendro <- load_data_dendrometry(path_to_fieldmapdb)
data_deadwood <- load_data_deadwood(path_to_fieldmapdb)
data_shoots <- load_data_shoots(path_to_fieldmapdb)
height_model <- load_height_models()
plotinfo <- load_plotinfo(path_to_fieldmapdb)
results_dendrometry <-
  calculate_dendrometry(
    data_dendro, data_deadwood, data_shoots, height_model, plotinfo
  )

describe("dendro_by_plot", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_dendrometry[["dendro_by_plot"]]),
      c("plottype", "plot_id", "year", "period",
        "number_of_tree_species", "number_of_trees_ha", "stem_number_ha",
        "basal_area_alive_m2_ha", "basal_area_dead_m2_ha",
        "vol_alive_m3_ha", "vol_dead_standing_m3_ha", "vol_bole_alive_m3_ha",
        "vol_bole_dead_m3_ha", "vol_log_m3_ha", "vol_deadw_m3_ha",
        "stems_per_tree")
    )
  })
})

describe("dendro_by_plot_species", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_dendrometry[["dendro_by_plot_species"]]),
      c("plottype", "plot_id", "year", "period", "species",
        "number_of_trees_ha", "stem_number_ha",
        "basal_area_alive_m2_ha", "basal_area_dead_m2_ha",
        "vol_alive_m3_ha", "vol_dead_standing_m3_ha", "vol_bole_alive_m3_ha",
        "vol_bole_dead_m3_ha", "vol_log_m3_ha", "vol_deadw_m3_ha",
        "stems_per_tree")
    )
  })
})

describe("dendro_by_diam_plot", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_dendrometry[["dendro_by_diam_plot"]]),
      c("plottype", "plot_id", "year", "period", "dbh_class_5cm",
        "stem_number_alive_ha", "stem_number_dead_ha",
        "basal_area_alive_m2_ha", "basal_area_dead_m2_ha",
        "vol_alive_m3_ha", "vol_dead_standing_m3_ha", "vol_bole_alive_m3_ha",
        "vol_bole_dead_m3_ha", "vol_log_m3_ha")
    )
  })
})

describe("dendro_by_diam_plot_species", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_dendrometry[["dendro_by_diam_plot_species"]]),
      c("plottype", "plot_id", "year", "period", "species", "dbh_class_5cm",
        "stem_number_alive_ha", "stem_number_dead_ha",
        "basal_area_alive_m2_ha", "basal_area_dead_m2_ha",
        "vol_alive_m3_ha", "vol_dead_standing_m3_ha", "vol_bole_alive_m3_ha",
        "vol_bole_dead_m3_ha", "vol_log_m3_ha")
    )
  })
})

describe("deadw_by_decay_plot", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_dendrometry[["deadw_by_decay_plot"]]),
      c("plottype", "plot_id", "year", "period", "decaystage",
        "vol_dead_standing_m3_ha", "vol_bole_dead_m3_ha", "vol_log_m3_ha")
    )
  })
})

describe("deadw_by_decay_plot_species", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_dendrometry[["deadw_by_decay_plot_species"]]),
      c("plottype", "plot_id", "year", "period", "species", "decaystage",
        "vol_dead_standing_m3_ha", "vol_bole_dead_m3_ha", "vol_log_m3_ha")
    )
  })
})

describe("compose_stem_data()", {
  stem_data <- compose_stem_data(data_dendro, data_shoots)
  data_dendro_extra <-
    load_data_dendrometry(path_to_fieldmapdb, extra_variables = TRUE)
  data_shoots_extra <-
    load_data_shoots(path_to_fieldmapdb, extra_variables = TRUE)
  stem_data_extra <-
    compose_stem_data(
      data_dendro_extra, data_shoots_extra, extra_variables = TRUE
    )
  it("output columns are correct", {
    expect_equal(
      colnames(stem_data),
      c("forest_reserve", "plot_id", "plottype",
        "period", "year", "date_dendro",
        "totalplotarea_ha", "plotarea_ha",
        "tree_measure_id", "old_id", "shoot_measure_id", "species",
        "dbh_mm", "height_m", "calc_height_fm",
        "alive_dead", "intact_snag", "ind_sht_cop", "decaystage",
        "dbh_class_5cm", "basal_area_m2",
        "crown_volume_reduction", "branch_length_reduction",
        "subcircle", "subcirclearea_ha",
        "r_A1", "r_A2", "r_A3", "r_A4",
        "dbh_min_a3", "dbh_min_a3_dead", "dbh_min_a4", "dbh_min_a4_dead",
        "dbh_min_core_area", "dbh_min_core_area_dead",
        "length_core_area_m", "width_core_area_m", "core_area_ha")
    )

    expect_equal(
      colnames(stem_data_extra),
      c("forest_reserve", "plot_id", "plottype",
        "period", "year", "date_dendro",
        "totalplotarea_ha", "plotarea_ha",
        "tree_measure_id", "old_id", "shoot_measure_id", "species",
        "dbh_mm", "height_m", "calc_height_fm",
        "alive_dead", "intact_snag", "ind_sht_cop", "decaystage",
        "dbh_class_5cm", "basal_area_m2",
        "crown_volume_reduction", "branch_length_reduction",
        "x_local", "y_local", "coppice_id",
        "iufro_hght", "iufro_vital", "iufro_socia",
        "remark", "common_remark",
        "subcircle", "subcirclearea_ha",
        "r_A1", "r_A2", "r_A3", "r_A4",
        "dbh_min_a3", "dbh_min_a3_dead", "dbh_min_a4", "dbh_min_a4_dead",
        "dbh_min_core_area", "dbh_min_core_area_dead",
        "length_core_area_m", "width_core_area_m", "core_area_ha")
    )
  })
})
