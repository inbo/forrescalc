context("test check funtions")

path_to_testdb <-
  system.file("example/testdb/mdb_bosres_test.sqlite", package = "forrescalc")

library(tibble)

describe("check_data_deadwood", {
  check_deadwood <- check_data_deadwood(path_to_testdb)
  check_deadwood <- check_deadwood[check_deadwood$period == 1, ]
  it("check max_diameter_mm", {
    expect_equal(
      check_deadwood[
        check_deadwood$lying_deadw_id == 11587 &
          check_deadwood$aberrant_field == "max_diameter_mm", ],
      tibble(
        plot_id = 101,
        lying_deadw_id = 11587,
        period = 1,
        aberrant_field = "max_diameter_mm",
        anomaly = "too low",
        aberrant_value = 10
      )
    )
  })
  it("check intact_fragment", {
    expect_equal(
      check_deadwood[
        check_deadwood$plot_id %in% c(101, 21000) &
          check_deadwood$aberrant_field == "intact_fragment", ],
      tibble(
        plot_id = 101,
        lying_deadw_id = c(11587, 11626, 11629, 21595),
        period = 1,
        aberrant_field = "intact_fragment",
        anomaly =
          c("missing", "not in lookuplist", rep("invalid for plottype", 2)),
        aberrant_value = c(NA, 40, 30, 10)
      )
    )
  })
  it("check alive_dead", {
    expect_equal(
      check_deadwood[
        check_deadwood$lying_deadw_id == 11587 &
          check_deadwood$aberrant_field == "alive_dead", ],
      tibble(
        plot_id = 101,
        lying_deadw_id = 11587,
        period = 1,
        aberrant_field = "alive_dead",
        anomaly = "tree alive",
        aberrant_value = 11
      )
    )
  })
  it("check decay_stage", {
    expect_equal(
      check_deadwood[
        check_deadwood$plot_id == 101 &
          check_deadwood$aberrant_field == "decay_stage", ],
      tibble(
        plot_id = 101,
        lying_deadw_id = c(11587, 11626, 11629),
        period = 1,
        aberrant_field = "decay_stage",
        anomaly = c("missing", "not in lookuplist", "tree not alive"),
        aberrant_value = c(NA, 17, 16)
      )
    )
  })
})

describe("check_data_herblayer", {
  check_herblayer <- check_data_herblayer(path_to_testdb)
  check_herblayer <- check_herblayer[check_herblayer$period == 1, ]
  it("check coverage_id", {
    expect_equal(
      check_herblayer[
        check_herblayer$plot_id == 101 &
          check_herblayer$aberrant_field == "coverage_id", ],
      tibble(
        plot_id = 101,
        subplot_id = 1,
        herblayer_id = c(119, 120),
        period = 1,
        aberrant_field = "coverage_id",
        anomaly = c("missing", "not in lookuplist"),
        aberrant_value = c(NA, 19)
      )
    )
  })
  it("check species", {
    expect_equal(
      check_herblayer[
        check_herblayer$plot_id == 101 &
          check_herblayer$aberrant_field == "species", ],
      tibble(
        plot_id = 101,
        subplot_id = 1,
        herblayer_id = c(119, 120),
        period = 1,
        aberrant_field = "species",
        anomaly = c("2 times the same species"),
        aberrant_value = 161
      )
    )
  })
  it("check browse_index", {
    expect_equal(
      check_herblayer[
        check_herblayer$plot_id == 101 &
          check_herblayer$aberrant_field == "browse_index", ],
      tibble(
        plot_id = 101,
        subplot_id = 1,
        herblayer_id = c(119, 120),
        period = 1,
        aberrant_field = "browse_index",
        anomaly = c("missing", "not in lookuplist"),
        aberrant_value = c(NA, 130)
      )
    )
  })
})

describe("check_data_plotdetails", {
  check_plotdetails <- check_data_plotdetails(path_to_testdb)
  check_plotdetails <- check_plotdetails[check_plotdetails$period == 1, ]
  it("check missing data CP", {
    expect_equal(
      check_plotdetails[check_plotdetails$plot_id == 20, ],
      tibble(
        plot_id = 20,
        period = 1,
        aberrant_field =
          c("forest_reserve", "date_dendro", "fieldteam",
            "ra1", "ra2", "ra3", "ra4", "area_ha"),
        anomaly = "missing",
        aberrant_value = NA_character_
      )
    )
  })
  it("check missing data CA", {
    expect_equal(
      check_plotdetails[check_plotdetails$plot_id == 30, ],
      tibble(
        plot_id = 30,
        period = 1,
        aberrant_field =
          c("forest_reserve", "date_dendro", "fieldteam",
            "length_core_area_m", "width_core_area_m", "area_ha"),
        anomaly = "missing",
        aberrant_value = NA_character_
      )
    )
  })
  it("check deviating date", {
    expect_equal(
      check_plotdetails[
        check_plotdetails$plot_id == 70 &
          check_plotdetails$anomaly != "missing", ],
      tibble(
        plot_id = 70,
        period = 1,
        aberrant_field = "date_dendro",
        anomaly = "deviating",
        aberrant_value = "2006-01-31 10:46:40"
      )
    )
  })
})

describe("check_data_plots", {
  check_plots <- check_data_plots(path_to_testdb)
  it("check plottype_id", {
    expect_equal(
      check_plots[check_plots$plot_id == c(40, 50), ],
      tibble(
        plot_id = c(40, 50),
        aberrant_field = "plottype_id",
        anomaly = c("missing", "not in lookuplist"),
        aberrant_value = c(NA, 80)
      )
    )
  })
})

describe("check_data_regeneration", {
  check_regeneration <- check_data_regeneration(path_to_testdb)
  check_regeneration <- check_regeneration[check_regeneration$period == 1, ]
  it("check missing data", {
    expect_equal(
      check_regeneration[check_regeneration$plot_id == 101, ],
      tibble(
        plot_id = 101,
        subplot_id = 1,
        period = 1,
        aberrant_field = c("date", "fieldteam"),
        anomaly = "missing",
        aberrant_value = NA_character_
      )
    )
  })
  it("check deviating date", {
    expect_equal(
      check_regeneration[check_regeneration$subplot_id == 155513, ],
      tibble(
        plot_id = 11000,
        subplot_id = 155513,
        period = 1,
        aberrant_field = "date",
        anomaly = "deviating",
        aberrant_value =  "2001-07-14 07:20:00"
      )
    )
  })
})

describe("check_data_regspecies", {
  check_regspecies <- check_data_regspecies(path_to_testdb)
  check_regspecies1 <- check_regspecies[check_regspecies$period == 1, ]
  check_regspecies3 <- check_regspecies[check_regspecies$period == 3, ]
  it("check heightclass", {
    expect_equal(
      check_regspecies1[
        check_regspecies1$plot_id == 101 &
          check_regspecies1$aberrant_field == "heightclass", ],
      tibble(
        plot_id = 101,
        subplot_id = 1,
        heightclass_id = c(142, 142, 142, 143),
        period = 1,
        regspecies_id = c(142, 145, 146, 150),
        aberrant_field = "heightclass",
        anomaly = "2 times the same height class",
        aberrant_value = 3000
      )
    )
  })
  it("check species", {
    expect_equal(
      check_regspecies1[
        check_regspecies1$plot_id == 101 &
          check_regspecies1$aberrant_field == "species", ],
      tibble(
        plot_id = 101,
        subplot_id = 1,
        heightclass_id = c(142, 143),
        period = 1,
        regspecies_id = c(146, 150),
        aberrant_field = "species",
        anomaly = "2 times the same species",
        aberrant_value = 39
      )
    )
  })
  it("check number", {
    expect_equal(
      check_regspecies1[
        check_regspecies1$regspecies_id == 150 &
          check_regspecies1$aberrant_field == "number", ],
      tibble(
        plot_id = 101,
        subplot_id = 1,
        heightclass_id = 143,
        period = 1,
        regspecies_id = 150,
        aberrant_field = "number",
        anomaly = "missing",
        aberrant_value = NA_integer_
      )
    )
  })
  it("check number_class", {  #error from original FM -> check if still wrong
    expect_equal(
      check_regspecies1[
        check_regspecies1$regspecies_id == 141 &
          check_regspecies1$aberrant_field == "number_class", ],
      tibble(
        plot_id = 101,
        subplot_id = 1,
        heightclass_id = 141,
        period = 1,
        regspecies_id = 141,
        aberrant_field = "number_class",
        anomaly = "missing",
        aberrant_value = NA_integer_
      )
    )
  })
  it("check game_damage_number", {
    expect_equal(
      check_regspecies3[
        check_regspecies3$plot_id == 101 &
          check_regspecies3$heightclass_id %in% c(1, 3) &
          check_regspecies3$aberrant_field == "game_damage_number", ],
      tibble(
        plot_id = 101,
        subplot_id = 1,
        heightclass_id = c(1, 3),
        period = 3,
        regspecies_id = c(1, 2),
        aberrant_field = "game_damage_number",
        anomaly = "higher than total number",
        aberrant_value = c(70, 20)
      )
    )
  })
})

describe("check_data_vegetation", {
  check_vegetation <- check_data_vegetation(path_to_testdb)
  check_vegetation <-
    check_vegetation[check_vegetation$plot_id %in% c(20, 60), ]
  it("check missing data", {
    expect_equal(
      check_vegetation[check_vegetation$subplot_id == 1, ],
      tibble(
        plot_id = 20,
        subplot_id = 1,
        period = 1,
        aberrant_field =
          c("date", "fieldteam",
            "moss_cover_id", "herb_cover_id", "shrub_cover_id", "tree_cover_id",
            "waterlayer_cover_id", "total_soildisturbance_game_id"),
        anomaly = "missing",
        aberrant_value = NA_character_
      )
    )
  })
  it("check data not in lookuplist", {
    expect_equal(
      check_vegetation[check_vegetation$subplot_id == 2, ],
      tibble(
        plot_id = 20,
        subplot_id = 2,
        period = 1,
        aberrant_field =
          c("date", "fieldteam",
            "moss_cover_id", "herb_cover_id", "shrub_cover_id", "tree_cover_id",
            "waterlayer_cover_id", "total_soildisturbance_game_id"),
        anomaly = c(rep("missing", 2), rep("not in lookuplist", 6)),
        aberrant_value = c(rep(NA, 2), rep("15", 6))
      )
    )
  })
  it("check invalid data", {
    expect_equal(
      check_vegetation[check_vegetation$subplot_id == 3, ],
      tibble(
        plot_id = 20,
        subplot_id = 3,
        period = 1,
        aberrant_field =
          c("date", "fieldteam",
            "moss_cover_id", "herb_cover_id", "shrub_cover_id", "tree_cover_id",
            "waterlayer_cover_id", "total_soildisturbance_game_id"),
        anomaly = c(rep("missing", 2), rep("invalid value", 6)),
        aberrant_value = c(rep(NA, 2), rep("20", 6))
      )
    )
  })
  it("check deviating date", {
    expect_equal(
      check_vegetation[check_vegetation$subplot_id == 5, ],
      tibble(
        plot_id = 60,
        subplot_id = 5,
        period = 1,
        aberrant_field = c("date", "fieldteam"),
        anomaly = c("deviating", "missing"),
        aberrant_value = c("2005-07-30 11:46:40", NA)
      )
    )
  })
})

describe("check_data_shoots", {
  check_shoots <- check_data_shoots(path_to_testdb)
  check_shoots <- check_shoots[check_shoots$period == 1, ]
  it("check missing data", {
    expect_equal(
      check_shoots[
        check_shoots$tree_measure_id == 55 & check_shoots$shoot_id == 3, ],
      tibble(
        plot_id = 21000,
        tree_measure_id = 55,
        shoot_id = 3,
        period = 1,
        aberrant_field =
          c("dbh_mm", "intact_snag", "decay_stage_shoots",
            "iufro_hght", "iufro_vital", "iufro_socia"),
        anomaly = "missing",
        aberrant_value = NA_integer_
      )
    )
    expect_equal(
      check_shoots[
        check_shoots$tree_measure_id == 55 & check_shoots$shoot_id == 2 &
          check_shoots$anomaly == "missing", ],
      tibble(
        plot_id = 21000,
        tree_measure_id = 55,
        shoot_id = 2,
        period = 1,
        aberrant_field = "dbh_mm",
        anomaly = "missing",
        aberrant_value = NA_integer_
      )
    )
  })
  it("check data shoot on no coppice", {
    expect_equal(
      check_shoots[check_shoots$tree_measure_id == 11559, ],
      tibble(
        plot_id = 101,
        tree_measure_id = 11559,
        shoot_id = 1,
        period = 1,
        aberrant_field =
          c("link_to_layer_trees", "ratio_dbh_height", "dbh_mm", "height_m",
            "intact_snag", "decay_stage_shoots", "iufro_hght", "iufro_vital",
            "iufro_socia"),
        anomaly =
          c("missing", "too high", "too high", "too low",
            rep("not in lookuplist", 5)),
        aberrant_value = c(NA, 628.6, 2001, 1, 12, 17, rep(50, 3))
      )
    )
  })
  it("check data shoot on alive tree", {
    expect_equal(
      check_shoots[
        check_shoots$tree_measure_id == 11557 & check_shoots$shoot_id == 3, ],
      tibble(
        plot_id = 101,
        tree_measure_id = 11557,
        shoot_id = 3,
        period = 1,
        aberrant_field =
          c("ratio_dbh_height", "height_m", "decay_stage_shoots",
            "iufro_hght", "iufro_vital", "iufro_socia"),
        anomaly =
          c("too low", "too high", rep("tree alive", 4)),
        aberrant_value = c(0, 55, 11, rep(40, 3))
      )
    )
  })
  it("check data shoot on dead tree", {
    expect_equal(
      check_shoots[
        check_shoots$tree_measure_id == 55 & check_shoots$shoot_id == 2, ],
      tibble(
        plot_id = 21000,
        tree_measure_id = 55,
        shoot_id = 2,
        period = 1,
        aberrant_field =
          c("dbh_mm", "decay_stage_shoots", "iufro_hght", "iufro_vital",
            "iufro_socia"),
        anomaly =
          c("missing", rep("tree not alive", 4)),
        aberrant_value = c(NA, 16, 10, 20, 30)
      )
    )
  })
})

describe("check_data_trees", {
  check_trees <- check_data_trees(path_to_testdb)
  check_trees <- check_trees[check_trees$period == 1, ]
  it("check location", {
    expect_equal(
      check_trees[check_trees$tree_measure_id %in% c(11559, 11554, 4053), ],
      tibble(
        plot_id = c(101, 101, 2006),
        tree_measure_id = c("11559", "11554", "4053"),
        period = 1,
        aberrant_field = "location",
        anomaly =
          c("tree not in A4", rep("tree not in A3", 2)),
        aberrant_value = NA_character_
      )
    )
  })
  it("check shoots linked with trees", {
    expect_equal(
      check_trees[check_trees$tree_measure_id == 11599 &
                    check_trees$aberrant_field == "link_to_layer_shoots", ],
      tibble(
        plot_id = 101,
        tree_measure_id = "11599",
        period = 1,
        aberrant_field = "link_to_layer_shoots",
        anomaly = "missing",
        aberrant_value = NA_character_
      )
    )
  })
  it("check number of stems", {
    expect_equal(
      check_trees[check_trees$tree_measure_id == 11557 &
                    check_trees$aberrant_field == "nr_of_stems", ],
      tibble(
        plot_id = 101,
        tree_measure_id = "11557",
        period = 1,
        aberrant_field = "nr_of_stems",
        anomaly = "incorrect",
        aberrant_value = "2"
      )
    )
  })
  it("check missing data", {
    expect_equal(
      check_trees[check_trees$tree_measure_id == 11603, ],
      tibble(
        plot_id = 101,
        tree_measure_id = "11603",
        period = 1,
        aberrant_field =
          c("dbh_mm", "species", "intact_snag", "ind_sht_cop", "decay_stage",
            "iufro_hght", "iufro_vital", "iufro_socia"),
        anomaly = "missing",
        aberrant_value = NA_character_
      )
    )
    expect_equal(
      check_trees[check_trees$tree_measure_id == 11604 &
                    check_trees$anomaly == "missing", ],
      tibble(
        plot_id = 101,
        tree_measure_id = "11604",
        period = 1,
        aberrant_field = c("dbh_mm", "species", "intact_snag", "alive_dead"),
        anomaly = "missing",
        aberrant_value = NA_character_
      )
    )
    expect_equal(
      check_trees[check_trees$tree_measure_id == 11602 &
                    check_trees$anomaly == "missing", ],
      tibble(
        plot_id = 101,
        tree_measure_id = "11602",
        period = 1,
        aberrant_field = c("dbh_mm", "height_m"),
        anomaly = "missing",
        aberrant_value = NA_character_
      )
    )
  })
  it("check dbh and height", {
    expect_equal(
      check_trees[check_trees$tree_measure_id %in% c(11600, 11601) &
                    grepl("^too ", check_trees$anomaly), ],
      tibble(
        plot_id = 101,
        tree_measure_id = c(rep("11600", 4), rep("11601", 2)),
        period = 1,
        aberrant_field =
          c("nr_of_stems", "ratio_dbh_height", "dbh_mm", "height_m",
            "ratio_dbh_height", "height_m"),
        anomaly =
          c("too low", "too high", "too high", "too low", "too low",
            "too high"),
        aberrant_value = c("0", "628.6", "2001", "1", "0", "55")
      )
    )
  })
  it("check not in lookuplist", {
    expect_equal(
      check_trees[check_trees$tree_measure_id == 11600 &
                    !grepl("^too ", check_trees$anomaly), ],
      tibble(
        plot_id = 101,
        tree_measure_id = "11600",
        shoot_id = 1,
        period = 1,
        aberrant_field =
          c("intact_snag", "alive_dead", "ind_sht_cop", "decay_stage",
            "iufro_hght", "iufro_vital", "iufro_socia", "commonremark"),
        anomaly = c(rep("not in lookuplist", 7), "tree not alive"),
        aberrant_value = c("12", "13", "13", "18", rep("60", 3), "150")
      )
    )
  })
  it("check data on no coppice (& alive)", {
    expect_equal(
      check_trees[grepl("11602", check_trees$tree_measure_id) &
                    check_trees$anomaly != "missing", ],
      tibble(
        plot_id = 101,
        tree_measure_id =
          c(rep("11602", 6), "11602_11597", rep("11601_11602_11597", 2)),
        period = 1,
        aberrant_field =
          c("ind_sht_cop", "decay_stage", "iufro_hght", "iufro_vital",
            "iufro_socia", "coppice_id", "coppice_id", "species",
            "location_shift"),
        anomaly =
          c("incorrect", rep("tree alive", 4), "unexpected (not missing)",
            "2 times the same coppice_id", "shifter in coppice tree",
            "walker in coppice tree"),
        aberrant_value =
          c("10", "12", rep("40", 3), "129", "129", "16_28_16", "3.66")
      )
    )
    expect_equal(
      check_trees[check_trees$tree_measure_id ==  11604 &
                    check_trees$anomaly != "missing", ],
      tibble(
        plot_id = 101,
        tree_measure_id = "11604",
        period = 1,
        aberrant_field =
          c("decay_stage", "iufro_hght", "iufro_vital", "iufro_socia"),
        anomaly = "tree no coppice",
        aberrant_value = c("17", rep("50", 3))
      )
    )
  })
  it("check data on coppice and dead", {
    expect_equal(
      check_trees[grepl("11601", check_trees$tree_measure_id)  &
                    !grepl("^too ", check_trees$anomaly), ],
      tibble(
        plot_id = 101,
        tree_measure_id = c(rep("11601", 5), rep("11601_11602_11597", 2)),
        period = 1,
        aberrant_field =
          c("link_to_layer_shoots", "decay_stage", "iufro_hght", "iufro_vital",
            "iufro_socia", "species", "location_shift"),
        anomaly =
          c("missing", rep("tree not alive", 4), "shifter in coppice tree",
            "walker in coppice tree"),
        aberrant_value = c(NA, "16", "10", "20", "30", "16_28_16", "3.66")
      )
    )
  })
})
