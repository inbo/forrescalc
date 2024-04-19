context("test load functions basics")

path_to_fieldmapdb <-
  system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")

describe("load_data_deadwood()", {
  data_deadwood <- load_data_deadwood(path_to_fieldmapdb)
  data_deadwood_extra <-
    load_data_deadwood(path_to_fieldmapdb, extra_variables = TRUE)
  it("output columns are correct", {
    expect_equal(
      colnames(data_deadwood),
      c("forest_reserve", "plot_id", "plottype",
        "period", "year", "date_dendro",
        "totalplotarea_ha", "plotarea_ha",
        "lying_deadw_id",
        "species", "decaystage", "intact_fragm",
        "calc_volume_m3", "calc_length_m", "total_length_m",
        "min_diam_mm", "max_diam_mm", "dbh_class_5cm",
         "r_A1", "r_A2", "r_A3", "r_A4",
        "length_core_area_m", "width_core_area_m", "core_area_ha"
        )
    )
    expect_equal(
      colnames(data_deadwood_extra),
      c("forest_reserve", "plot_id", "plottype",
        "period", "year", "date_dendro",
        "totalplotarea_ha", "plotarea_ha",
        "lying_deadw_id",
        "species", "decaystage", "intact_fragm",
        "calc_volume_m3", "calc_length_m", "total_length_m",
        "min_diam_mm", "max_diam_mm", "dbh_class_5cm",
        "remark", "common_remark",
        "r_A1", "r_A2", "r_A3", "r_A4",
        "length_core_area_m", "width_core_area_m", "core_area_ha"
        )
    )
  })
})

describe("load_data_dendrometry()", {
  data_dendro <- load_data_dendrometry(path_to_fieldmapdb)
  data_dendro_extra <-
    load_data_dendrometry(path_to_fieldmapdb, extra_variables = TRUE)
  it("output columns are correct", {
    expect_equal(
      colnames(data_dendro),
      c("forest_reserve", "plot_id", "plottype",
        "period", "year", "date_dendro",
        "totalplotarea_ha", "plotarea_ha",
        "tree_measure_id", "old_id", "species",
        "dbh_mm", "height_m", "calc_height_fm",
        "alive_dead", "intact_snag", "ind_sht_cop", "decaystage",
        "nr_of_stems", "dbh_class_5cm",
        "crown_volume_reduction", "branch_length_reduction",
        "subcircle", "subcirclearea_ha",
        "r_A1", "r_A2", "r_A3", "r_A4",
        "dbh_min_a3", "dbh_min_a3_dead", "dbh_min_a4", "dbh_min_a4_dead",
        "dbh_min_core_area", "dbh_min_core_area_dead",
        "length_core_area_m", "width_core_area_m", "core_area_ha")
    )
    expect_equal(
      colnames(data_dendro_extra),
      c("forest_reserve", "plot_id", "plottype",
        "period", "year", "date_dendro",
        "totalplotarea_ha", "plotarea_ha",
        "tree_measure_id", "old_id", "species",
        "dbh_mm", "height_m", "calc_height_fm",
        "alive_dead", "intact_snag", "ind_sht_cop", "decaystage",
        "nr_of_stems", "dbh_class_5cm",
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

describe("load_data_herblayer()", {
  data_herblayer <- load_data_herblayer(path_to_fieldmapdb)
  it("output columns are correct", {
    expect_equal(
      colnames(data_herblayer),
      c("forest_reserve", "plot_id", "plottype", "subplot_id",
        "period", "year", "date_vegetation",
        "totalplotarea_ha", "plotarea_ha",
        "species", "coverage_id",
        "browse_index_id", "coverage_class_average_perc",
        "length_core_area_m", "width_core_area_m", "core_area_ha",
        )
    )
  })
})

describe("load_data_regeneration()", {
  data_regeneration <- load_data_regeneration(path_to_fieldmapdb)
  it("output columns are correct", {
    expect_equal(
      colnames(data_regeneration),
      c("forest_reserve", "plot_id", "plottype", "subplot_id",
        "period", "year", "date_regeneration",
        "totalplotarea_ha", "plotarea_ha",
        "height_class", "species", "number_class",
        "nr_of_regeneration","approx_nr_regeneration",
        "min_number_of_regeneration", "max_number_of_regeneration",
        "rubbing_damage_number", "rubbing_damage_perc",
        "subcircle", "subcirclearea_ha",
        "r_A1", "r_A2",
        "length_core_area_m", "width_core_area_m", "core_area_ha")
    )
  })
})

describe("load_data_shoots()", {
  data_shoots <- load_data_shoots(path_to_fieldmapdb)
  data_shoots_extra <-
    load_data_shoots(path_to_fieldmapdb, extra_variables = TRUE)
  it("output columns are correct", {
    expect_equal(
      colnames(data_shoots),
      c("plot_id", "period", "tree_measure_id", "shoot_measure_id",
        "dbh_mm", "height_m", "intact_snag", "decaystage")
    )
    expect_equal(
      colnames(data_shoots_extra),
      c("plot_id", "period", "tree_measure_id", "shoot_measure_id",
        "dbh_mm", "height_m", "intact_snag", "decaystage",
        "iufro_hght_shoots", "iufro_vital_shoots", "iufro_socia_shoots",
        "remark_shoots", "common_remark_shoots")
    )
  })
})

describe("load_data_vegetation()", {
  data_vegetation <- load_data_vegetation(path_to_fieldmapdb)
  it("output columns are correct", {
    expect_equal(
      colnames(data_vegetation),
      c("forest_reserve", "plot_id", "plottype", "subplot_id",
        "period", "year_main_survey", "date_vegetation",
        "totalplotarea_ha", "plotarea_ha",
        "total_moss_cover_id", "total_herb_cover_id", "total_shrub_cover_id",
        "total_tree_cover_id", "total_waterlayer_cover_id",
        "total_soildisturbance_game_id",
        "moss_cover_interval", "moss_cover_min", "moss_cover_max",
        "herb_cover_interval", "herb_cover_min", "herb_cover_max",
        "shrub_cover_interval", "shrub_cover_min", "shrub_cover_max",
        "tree_cover_interval", "tree_cover_min", "tree_cover_max",
        "waterlayer_cover_interval",
        "waterlayer_cover_min", "waterlayer_cover_max",
        "soildisturbance_game_cover_interval",
        "soildisturbance_game_cover_min", "soildisturbance_game_cover_max",
        "moss_cover_mid", "herb_cover_mid", "shrub_cover_mid", "tree_cover_mid",
        "waterlayer_cover_mid", "soildisturbance_game_cover_mid",
        "length_core_area_m", "width_core_area_m", "core_area_ha"
        )
    )
  })
})

describe("load_plotinfo()", {
  plotinfo <- load_plotinfo(path_to_fieldmapdb)
  it("output columns are correct", {
    expect_equal(
      colnames(plotinfo),
      c("forest_reserve", "plot_id", "plottype",
        "period", "survey_number", "year_dendro",
        "survey_trees", "survey_deadw", "survey_veg", "survey_reg",
        "game_impact_veg", "game_impact_reg", "data_processed",
        )
    )
  })
})

path_to_height_models <-
  system.file("example/height_models", package = "forrescalc")

describe("load_height_models()", {
  height_models <- load_height_models(path_to_height_models)
  it("output columns are correct", {
    expect_equal(
      colnames(height_models),
      c("forest_reserve", "plottype", "period", "species", "model", "P1", "P2")
    )
  })
})
