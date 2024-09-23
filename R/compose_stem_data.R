#' Combine dendro data and shoot data to give detailed stem data
#'
#' This function replaces in the given dendrometric data (result from function
#' `load_data_dendrometry()`) the diameters, height, decay stage and info on
#' intact/snag from coppice trees by their separate stems given in the shoot
#' data (result from function `load_data_shoots()`).
#'
#' @param extra_variables Should additional variables such as `iufro_hght`,
#' `iufro_vital`, `iufro_socia`, `remark` and `common_remark` be added?
#' Default is FALSE (no).
#' ATTENTION: some variables as IUFRO-classes and (common-)remark are
#' - for coppice - collected at shoot level.
#' To include these extra variables, it is necessary to indicate this argument
#' in both load-functions (`load_data_dendrometry()` and `load_data_shoots()`):
#' extra_variables = TRUE.
#' @inheritParams calculate_dendrometry
#' @inheritParams load_data_shoots
#'
#' @return Dataframe with shoot data
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_dendro <- load_data_dendrometry(path_to_fieldmapdb)
#' data_shoots <- load_data_shoots(path_to_fieldmapdb)
#' compose_stem_data(data_dendro, data_shoots)
#'
#' #to include iufro-classes and other additional variables:
#' data_dendro <-
#'   load_data_dendrometry(path_to_fieldmapdb, extra_variables = TRUE)
#' data_shoots <-
#'   load_data_shoots(path_to_fieldmapdb, extra_variables = TRUE)
#' compose_stem_data(data_dendro, data_shoots, extra_variables = TRUE)
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows filter inner_join mutate relocate select
#' @importFrom assertthat has_name
#'
compose_stem_data <-
  function(data_dendro, data_shoots, extra_variables = FALSE) {
  extra_vars <- c("iufro_hght", "iufro_vital", "iufro_socia",
                  "remark", "common_remark")
  extra_vars_shoots <- c("iufro_hght_shoots", "iufro_vital_shoots",
                         "iufro_socia_shoots",
                         "remark_shoots", "common_remark_shoots")
  if (extra_variables) {
  assert_that(
    has_name(data_dendro, extra_vars),
    msg =
      "data_dendro should contain extra variables as iufroclasses and (common_)remark" #nolint: line_length_linter
  )
  assert_that(
    has_name(data_shoots, extra_vars_shoots),
    msg =
      "data_shoots should contain extra variables as iufroclasses and (common_)remark" #nolint: line_length_linter
  )
  } else {
    if (has_name(data_dendro, extra_vars)) {
      data_dendro <- data_dendro %>% select(-all_of(extra_vars))
    }
    if (has_name(data_shoots, extra_vars_shoots)) {
      data_shoots <- data_shoots %>% select(-all_of(extra_vars_shoots))
    }
  }
  attributes <-
    compare_attributes(
      data_dendro, data_shoots, "data_dendro", "data_shoots"
    )
  #omit data that could be misinterpreted if data on shoot level are added
  data_dendro_relevant <- data_dendro %>%
    select(
      -"nr_of_stems", -"dbh_class_5cm"
    )
  stem_data <- data_dendro_relevant %>%
    filter(.data$ind_sht_cop != 12) %>%
    bind_rows(
      data_dendro_relevant %>%
        select(-"dbh_mm", -"height_m",
               -"intact_snag", -"decaystage") %>%
        filter(.data$ind_sht_cop == 12) %>%
        inner_join(data_shoots, by = c("plot_id", "tree_measure_id", "period"))
    ) %>%
    mutate(
      dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm),
      basal_area_m2 = pi * (.data$dbh_mm / 2000) ^ 2
    ) %>%
    relocate("shoot_measure_id", .after = "old_id") %>%
    relocate(all_of(c("dbh_class_5cm", "basal_area_m2")), .after = "decaystage")

  if (
    has_name(
      stem_data,
      c("iufro_hght", "iufro_vital", "iufro_socia", "iufro_hght_shoots",
        "iufro_vital_shoots", "iufro_socia_shoots",
        "remark_shoots", "common_remark_shoots")
    )
  ) {
    stem_data <- stem_data %>%
      mutate(
        iufro_hght =
          ifelse(is.na(.data$iufro_hght_shoots),
                 .data$iufro_hght, .data$iufro_hght_shoots),
        iufro_hght_shoots = NULL,
        iufro_vital =
          ifelse(is.na(.data$iufro_vital_shoots),
                 .data$iufro_vital, .data$iufro_vital_shoots),
        iufro_vital_shoots = NULL,
        iufro_socia =
          ifelse(is.na(.data$iufro_socia_shoots),
                 .data$iufro_socia, .data$iufro_socia_shoots),
        iufro_socia_shoots = NULL,
        remark =
          ifelse(is.na(.data$remark_shoots),
                 .data$remark, .data$remark_shoots),
        remark_shoots = NULL,
        common_remark =
          ifelse(is.na(.data$common_remark_shoots),
                 .data$common_remark, .data$common_remark_shoots),
        common_remark_shoots = NULL,
      )
  }

  attr(stem_data, "database") <- attributes[["attr_database"]]
  attr(stem_data, "forrescalc") <- attributes[["attr_forrescalc"]]

  return(stem_data)
}
