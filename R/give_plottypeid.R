#' check input and give id of plottype
#'
#' Internal helper function that checks the input of plottype and gives the
#' ID from the Fieldmap database.
#'
#' @inheritParams load_data_dendrometry
#'
#' @noRd
#'
#'
give_plottypeid <- function(plottype = c(NA, "CP", "KV")) {
  match.arg(plottype)
  plottypeid <-
    ifelse(
      is.na(plottype),
      "20,30",
      ifelse(
        plottype == "CP",
        "20",
        "30"
      )
    )
  return(plottypeid)
}
