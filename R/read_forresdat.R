#' load tables from git repository forresdat
#'
#' This function reads a table in git2rdata format from git repository forresdat
#' (or another git repository dependent on the given repo path).
#'
#' @param tablename name of the table that should be read
#' @param repo_path name and path of local git repository from which data should be retrieved
#' @param join_plotinfo should table plotinfo be joined to the chosen table to
#' add columns plottype and forest_reserve?  Default is TRUE.  (This is only
#' possible if the given table contains a column plot_id, so this parameter
#' should be put FALSE if this column is absent.)
#' @param plottype Data of which 'plottype' (used method) should be retrieved?
#' Default is 'CP' or 'circle plot', alternatively 'CA' or 'core area' could be
#' chosen.
#'
#' @return A dataframe with the specified table, default columns plottype and forest_reserve.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' read_forresdat(tablename = "dendro_by_plot", repo_path = "C:/gitrepo/forresdat")
#' }
#'
#' @export
#'
#' @importFrom git2rdata pull read_vc repository
#' @importFrom assertthat assert_that has_name
#'
read_forresdat <-
  function(tablename, repo_path, join_plotinfo = TRUE, plottype = c("CP", "CA")) {
  assert_that(is.logical(join_plotinfo))
  plottype <- match.arg(plottype)
  repo <- repository(repo_path)
  pull(repo, credentials = get_cred(repo))
  dataset <- read_vc(file = paste0("data/", tablename), root = repo)
  if (join_plotinfo) {
    assert_that(
      has_name(dataset, "plot_id"),
      msg = "No column 'plot_id' in the requested table, please add 'join_plotinfo = FALSE'" #nolint
    )
    if (has_name(dataset, "period")) {
      dataset <- dataset %>%
        left_join(
          read_vc(file = "data/plotinfo", root = repo) %>%
            select(-.data$year),
          by = c("plot_id", "period")
        ) %>%
        filter(.data$plottype == plottype)
    } else {
      dataset <- dataset %>%
        left_join(
          read_vc(file = "data/plotinfo", root = repo) %>%
            select(-.data$year),
          by = "plot_id"
        ) %>%
        filter(.data$plottype == plottype)
    }
  } else {
    warning(
      paste(
        "Data include all plottypes, use 'join_plotinfo = TRUE' if you only want data of plottype", #nolint
        plottype
      )
    )
  }
  return(dataset)
}
