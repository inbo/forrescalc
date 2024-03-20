#' check userinput and convert to final part of query
#'
#' Internal helper function that checks if the input values are present in the
#' fieldmap database (with help of function check_input()) and translates
#' this input to a query part that makes the desired selection if pasted as a
#' final part of a query.  This query can contain a join!
#'
#' @inheritParams load_data_dendrometry
#' @param survey_name column name in table PlotDetails_xeSet that indicates if
#' survey is done
#'
#' @importFrom assertthat assert_that
#'
#' @noRd
#'
translate_input_to_selectquery <-
  function(database, plottype, forest_reserve, processed, survey_name) {
    if (!is.na(plottype)) {
      check_input(plottype, database, "qPlotType", "Value3")
      selection <-
        paste0(" WHERE qPlotType.Value3 in ('", plottype, "')")
    } else {
      selection <- ""
    }
    if (!is.na(forest_reserve)) {
      check_input(
        forest_reserve, database, "PlotDetails_1eSet", "ForestReserve",
        "PlotDetails_2eSet"
      )
      selection <- ifelse(selection == "", "WHERE", paste(selection, "AND"))
      selection <-
        paste0(selection, " pd.ForestReserve in ('", forest_reserve, "')")
    }
    assert_that(is.logical(processed))
    if (processed) {
      selection <- ifelse(selection == "", "WHERE", paste(selection, "AND"))
      selection <-
        paste0(selection, " pd.DataProcessed_YN = 10 AND pd.", survey_name,
               " = 10")
    }
    return(selection)
}
