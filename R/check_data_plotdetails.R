#' check table `Plotdetails` from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table `Plotdetails`
#' (of all periods) from the given database and
#' checks for missing data or wrong input.
#'
#' @inheritParams check_data_trees
#'
#' @return Dataframe with inconsistent data with ID's and additional columns
#' `aberrant_field` (which column is wrong) and `anomaly` (what is wrong with
#' the input)
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' check_data_plotdetails(path_to_fieldmapdb)
#' check_data_plotdetails(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% across bind_rows filter group_by ungroup mutate select
#' @importFrom lubridate year
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect ends_with starts_with
#'
check_data_plotdetails <- function(database, forest_reserve = "all") {
  selection <-
    ifelse(
      forest_reserve == "all", "",
      paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
    )
  query_plotdetails <-
    "SELECT pd.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Date_Dendro_%1$deSet AS date_dendro,
      pd.FieldTeam_Dendro_%1$deSet AS fieldteam,
      pd.rA1 AS ra1,
      pd.rA2 AS ra2,
      pd.rA3 AS ra3,
      pd.rA4 AS ra4,
      pd.LengthCoreArea_m AS length_core_area_m,
      pd.WidthCoreArea_m AS width_core_area_m,
      pd.Area_ha AS area_ha
    FROM (Plots
        INNER JOIN Plotdetails_%1$deSet pd ON Plots.ID = pd.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID
    %3$s;"

  query_plotdetails_1986 <- sprintf(
    "SELECT pd.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Date_Dendro_1986 AS date_dendro,
      pd.FieldTeam_Dendro_1eSet AS fieldteam,
      pd.rA1 AS ra1,
      pd.rA2 AS ra2,
      pd.rA3 AS ra3,
      pd.rA4 AS ra4,
      pd.LengthCoreArea_m AS length_core_area_m,
      pd.WidthCoreArea_m AS width_core_area_m,
      pd.Area_ha AS area_ha
    FROM (Plots
        INNER JOIN Plotdetails_1986 pd ON Plots.ID = pd.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID
    %1$s;",
    selection
  )

  data_plotdetails <-
    query_database(database, query_plotdetails, selection = selection)
  con <- connect_to_database(database)
  data_plotdetails_1986 <- dbGetQuery(con, query_plotdetails_1986) %>%
    mutate(period = 0)
  if (nrow(data_plotdetails_1986) > 0) {
    if (inherits(con, "SQLiteConnection")) {
      data_plotdetails_1986 <- data_plotdetails_1986 %>%
        mutate(
          date_dendro = as.POSIXct(.data$date_dendro, origin = "1970-01-01")
        )
    }
    data_plotdetails <- data_plotdetails %>%
      bind_rows(
        data_plotdetails_1986
      )
  }
  dbDisconnect(con)

  incorrect_plotdetails <- data_plotdetails %>%
    group_by(.data$forest_reserve, .data$period, .data$plottype) %>%
    mutate(
      forest_reserve_date = median(.data$date_dendro)
    ) %>%
    ungroup() %>%
    mutate(
      field_forest_reserve =
        ifelse(is.na(.data$forest_reserve), "missing", NA),
      field_date_dendro =
        ifelse(is.na(.data$date_dendro), "missing", NA),
      field_date_dendro =
        ifelse(
          is.na(.data$field_date_dendro) &
            year(.data$date_dendro) != year(.data$forest_reserve_date),
          "deviating",
          .data$field_date_dendro
        ),
      field_fieldteam = ifelse(is.na(.data$fieldteam), "missing", NA),
      field_ra1 =
        ifelse(is.na(.data$ra1) & .data$plottype == "CP", "missing", NA),
      field_ra2 =
        ifelse(is.na(.data$ra2) & .data$plottype == "CP", "missing", NA),
      field_ra3 =
        ifelse(is.na(.data$ra3) & .data$plottype == "CP", "missing", NA),
      field_ra4 =
        ifelse(is.na(.data$ra4) & .data$plottype == "CP", "missing", NA),
      field_length_core_area_m =
        ifelse(
          is.na(.data$length_core_area_m) & .data$plottype == "CA", "missing",
          NA
        ),
      field_width_core_area_m =
        ifelse(
          is.na(.data$width_core_area_m) & .data$plottype == "CA", "missing",
          NA
        ),
      field_area_ha = ifelse(is.na(.data$area_ha), "missing", NA)
    ) %>%
    pivot_longer(
      cols = c(starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      plottype = NULL,
      forest_reserve = NA_character_,
      date_dendro = as.character(.data$date_dendro),
      fieldteam = as.character(.data$fieldteam),
      forest_reserve_date = NULL,
      across(starts_with("ra"), as.character),
      across(ends_with("_core_area_m"), as.character),
      area_ha = as.character(.data$area_ha)
    ) %>%
    pivot_longer(
      cols = !c("plot_id", "period", "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(.data$aberrant_field == .data$varname) %>%
    select(-"varname")

  return(incorrect_plotdetails)
}
