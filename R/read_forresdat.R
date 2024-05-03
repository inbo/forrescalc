#' load tables from git repository forresdat
#'
#' This function reads a table in tsv format from git repository forresdat.
#' Data available in forresdat only contains observations, so no records with
#' zero values are added for for instance species that were not observed and
#' hence absent.
#' These zero value records can easily be added by using the function
#' `add_zeros()`.
#' To load table plotinfo, set argument `join_plotinfo = FALSE`.
#'
#' @param tablename name of the table that should be read
#' @param join_plotinfo should table plotinfo be joined to the chosen table to
#' add columns forest_reserve, survey_dendro/deadw/reg/veg (TRUE or
#' FALSE) and data_processed (TRUE or FALSE)?
#' Default is TRUE.
#' (This is only possible if the given table contains a column plot_id,
#' so this parameter should be put FALSE if this column is absent.)
#' Must be FALSE if you want to load the table "plotinfo" itself.
#' @param plottype Data of which 'plottype' (used method) should be retrieved?
#' Default is 'CP' or 'circle plot', alternatively 'CA' or 'core area', or 'all'
#' (retrieve both circle plots and core areas) could be chosen.
#'
#' @return A dataframe with the specified table, default columns plottype,
#' forest_reserve, survey_dendro/deadw/reg/veg (TRUE or FALSE) and
#' data_processed (TRUE or FALSE).
#' To be able to recall the version of the data, this dataframe contains
#' an attribute with the sha of the commit of forresdat from which the data
#' are taken.
#'
#' @examples
#' library(forrescalc)
#' data_dendro <- read_forresdat(tablename = "dendro_by_plot")
#' data_dendro
#' attr(data_dendro, "forresdat")
#'
#' @export
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr select
#' @importFrom readr read_tsv
#'
read_forresdat <-
  function(tablename, join_plotinfo = TRUE, plottype = c("CP", "CA", "all")) {
  assert_that(is.logical(join_plotinfo))
  var_plottype <- match.arg(plottype)
  url <-
    sprintf(
      "https://raw.githubusercontent.com/inbo/forresdat/master/data/%s.tsv",
      tablename
    )
  dataset <- read_tsv(url)
  if (has_name(dataset, "plottype") && var_plottype != "all") {
    dataset <- dataset %>%
      filter(.data$plottype == var_plottype)
  } else {
    if (var_plottype != "all") {
      warning(
        sprintf(
          "As table '%s' has no field 'plottype', the output table contains all records, not only those with plottype %s.", #nolint: line_length_linter
          tablename, var_plottype
        )
      )
    }
  }
  if (join_plotinfo) {
    assert_that(
      has_name(dataset, "plot_id"),
      msg =
        "No column 'plot_id' in the requested table, please add 'join_plotinfo = FALSE'" #nolint: line_length_linter
    )
    if (has_name(dataset, "plottype")) {
      dataset <- dataset %>%
        select(-"plottype")
    }
    url_plotinfo <-
      "https://raw.githubusercontent.com/inbo/forresdat/master/data/plotinfo.tsv" #nolint: line_length_linter
    if (has_name(dataset, "period")) {
      dataset <- dataset %>%
        left_join(
          read_tsv(url_plotinfo),
          by = c("plot_id", "period")
        )
    } else {
      dataset <- dataset %>%
        left_join(
          read_tsv(url_plotinfo),
          by = "plot_id"
        )
    }
  }
  warning("The dataset only contains presence data and lacks zero observations (except for 1 observation per plot_id and period to indicate that observations are done).  Please use function add_zeros() to add zero observations when needed.") #nolint

  commit <- fromJSON("https://api.github.com/repos/inbo/forresdat/commits?")
  attr(dataset, "forresdat") <-
    paste("forresdat commit", commit$sha[1])

  return(dataset)
}
