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
        aberrant_value = NA_integer_
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
        aberrant_value = NA_integer_
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
        aberrant_value = NA_integer_
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
