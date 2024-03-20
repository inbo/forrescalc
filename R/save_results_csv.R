#' save results of calculations as csv-files
#'
#' This function saves the results from calculations in the forrescalc package
#' (or any other named list with dataframes) in a predefined folder.
#' List item names will be used to name each of the tables, which contain as
#' a content the different dataframes.
#'
#' @param results results from calculations in package forrescalc as a named
#'   list
#' @param output_dir name of output folder including path in which results
#'   should be saved
#'
#' @return No value is returned, data are saved in the specified folder
#'
#' @examples
#' library(forrescalc)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' path_to_plotlevel_csv <- getwd()
#' data_regeneration <- load_data_regeneration(database = path_to_fieldmapdb)
#' regeneration <- calculate_regeneration(data_regeneration)
#' save_results_csv(results = regeneration, output_dir = path_to_plotlevel_csv)
#'
#' @export
#'
#' @importFrom utils write.csv2
#'

save_results_csv <- function(results, output_dir) {
  for (tablename in names(results)) {
    write.csv2(
      results[[tablename]], file = paste0(output_dir, "/", tablename, ".csv"))
  }
}
