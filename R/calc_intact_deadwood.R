#' Calculate bole and crown volume of intact deadwood
#'
#' In Core Areas, some lying deadwood is marked as 'complete tree' by giving
#' variable `intact_fragm` value 10 (intact) instead of 20 (fragment) to save
#' time (while in general all fragments are measured separately).
#' This function calculates the total volume (sum of bole and crown volume) for
#' this intact deadwood and keeps the initial volume in case of fragments.
#'
#' @param data_deadwood dataframe on logs with variables `plot_id`, `plottype`,
#' `date_dendro`, `species`, `decaystage`, `intact_fragm`, `calc_volume_m3`,
#' `period` and `year` (output of function `load_data_deadwood()`), in which
#' `calc_volume_m3` should be replaced by a more precise calculation
#'
#' @return A similar dataframe (data_deadwood) in which the volume of intact
#' deadwood is replaced by a volume calculated based on tariffs.
#' Intermediate results `vol_crown_m3` and `vol_bole_m3` are added as columns
#' (which are NA in case of deadwood fragments).
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_deadwood <- load_data_deadwood(path_to_fieldmapdb)
#' calc_intact_deadwood(data_deadwood)
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows filter mutate rename select
#'
calc_intact_deadwood <- function(data_deadwood) {

  check_forrescalc_version_attr(data_deadwood)
  data_deadwood_intact <- data_deadwood %>%
    filter(.data$intact_fragm == 10) %>%
    rename(
      dbh_mm = "max_diam_mm",
      calc_height_m = "calc_length_m"
    ) %>%
    mutate(
      ind_sht_cop = 10,
      intact_snag = 11
    )
  data_deadwood_intact_recalc <- calc_stem_volume(data_deadwood_intact) %>%
    rename(
      max_diam_mm = "dbh_mm",
      calc_length_m = "calc_height_m"
    ) %>%
    mutate(
      calc_volume_m3 = .data$vol_crown_m3 + .data$vol_bole_m3
    ) %>%
    select(-"ind_sht_cop", -"intact_snag")
  data_deadwood_recalc <- data_deadwood %>%
    filter(.data$intact_fragm != 10) %>%
    bind_rows(data_deadwood_intact_recalc)

  return(data_deadwood_recalc)
}
