#' moves data from fieldmap database to SQLite database
#'
#' This function moves all relevant tables from an unindexed Access database to a SQLite database with indexes, for performance reasons. (Not finished yet and not exported!)
#'
#' @param source_db name of fieldmap/access database (with specific fieldmap structure) including path
#' @param new_db name and path of SQLite database to be made
#'
#' @return A SQLite database with the specified name is saved at the specified path
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#'
access_to_sqlite <- function(source_db, new_db) {
  query_plots <-
    "SELECT
      ID, Area_m2, Plottype, Area_ha, Date_dendro_1eSet, Date_vegetation_1eSet,
      Date_dendro_2eSet, Date_vegetation_2eSet
    FROM Plots;"
  query_trees <-
    "SELECT
      IDPlots, ID, DBH_mm, Height_m, Species, IntactSnag, AliveDead, IndShtCop,
      CoppiceID, DecayStage, TreeNumber, Vol_stem_m3, Vol_crown_m3, Vol_tot_m3,
      BasalArea_m2
    FROM Trees;"
  query_trees_2eSET <-
    "SELECT
      IDPlots, ID, DBH_mm, Height_m, Species, IntactSnag, AliveDead, IndShtCop,
      CoppiceID, DecayStage, TreeNumber, Vol_stem_m3, Vol_crown_m3, Vol_tot_m3,
      BasalArea_m2
    FROM Trees_2eSET;"
  query_regeneration <-
    "SELECT IDPlots, Area_m2
    FROM Regeneration"
  query_regeneration_2eSet <-
    "SELECT IDPlots, Area_m2
    FROM Regeneration_2eSet"
  query_height_class <-
    "SELECT IDPlots, ID, HeightClass
    FROM HeightClass"
  query_height_class_2eSet <-
    "SELECT IDPlots, ID, HeightClass
    FROM HeightClass_2eSet"
  query_regspecies <-
    "SELECT IDPlots, IDHeightClass, ID, Species, NumberClass, Number
    FROM RegSpecies"
  query_regspecies_2eSet <-
    "SELECT IDPlots, IDHeightClass, ID, Species, NumberClass, Number
    FROM RegSpecies_2eSet"


  con <- odbcConnectAccess2007(source_db)
  Plots <- sqlQuery(con, query_plots, stringsAsFactors = FALSE)
  Trees <- sqlQuery(con, query_trees, stringsAsFactors = FALSE)
  Trees_2eSET <- sqlQuery(con, query_trees_2eSET, stringsAsFactors = FALSE)
  Regeneration <- sqlQuery(con, query_regeneration, stringsAsFactors = FALSE)
  Regeneration_2eSet <-
    sqlQuery(con, query_regeneration_2eSet, stringsAsFactors = FALSE)
  HeightClass <- sqlQuery(con, query_height_class, stringsAsFactors = FALSE)
  HeightClass_2eSet <-
    sqlQuery(con, query_height_class_2eSet, stringsAsFactors = FALSE)
  RegSpecies <- sqlQuery(con, query_regspecies, stringsAsFactors = FALSE)
  RegSpecies_2eSet <-
    sqlQuery(con, query_regspecies_2eSet, stringsAsFactors = FALSE)
  odbcClose(con)

  #en dan opslaan in SQLite en de nodige indexen aanmaken
}
