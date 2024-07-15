# This file generates (or updates) a minimal example dataset that is added to
# the package to provide working examples and for testing reasons.
# It should be updated after structural changes are made in the FM database.

path_to_fieldmap <-
  "C:/R/bosreservatendb/MDB_BOSRES_selectieEls/FieldMapData_dB_Els_deel2.accdb"

library(DBI)
library(dplyr)

query_table <-
  function(
    table, con = con_fm, id = "ID",
    plot_id = "101, 11000, 21000, 141100, 204, 1005, 2006", top_x = 3
  ) {
  columns <- dbListFields(con, table)
  columns <- columns[!grepl("^FM", columns) & !grepl("^FieldStatus$", columns)]
  filter_species <-
    ifelse(
      any(columns == "Species"),
       "AND t1.Species IN (6, 7, 16, 26, 28, 87, 131, 161)",
       ""
    )
  add_species <-
    ifelse(
      any(columns == "Species"),
      "AND t2.Species = t1.Species",
      ""
    )
  columns <- paste("t1.", columns, sep = "", collapse = ", ")
  query <-
    gsub("\\n", " ",
         sprintf(
           "SELECT %1$s FROM %2$s t1
            WHERE t1.IDPlots IN (%6$s) %4$s
            AND t1.%3$s IN (
              SELECT DISTINCT TOP %7$s t2.%3$s FROM %2$s t2
              WHERE t2.IDPlots = t1.IDPlots %5$s
              ORDER BY t2.%3$s
            );",
           columns, table, id, filter_species, add_species, plot_id, top_x
         )
    )
  result <- dbGetQuery(con, query)
  return(result)
}

query_reltable <- function(table, con = con_fm, related_table, id = "ID") {
  columns <- dbListFields(con, table)
  columns <- columns[!grepl("^FM", columns) & !grepl("^FieldStatus$", columns)]
  columns <- paste("t1.", columns, sep = "", collapse = ", ")
  result <- NA
  for (plot_id in unique(related_table$IDPlots)) {
    related_table_plot <- related_table %>%
      filter(.data$IDPlots == plot_id)
    id_values <- paste(unique(related_table_plot$ID), collapse = ", ")
    query <-
      gsub("\\n", " ",
           sprintf(
             "SELECT %1$s FROM %2$s t1
              WHERE t1.IDPlots IN (%5$s)
              AND t1.%3$s IN (%4$s);",
             columns, table, id, id_values, plot_id
           )
      )
    if (length(result) == 1 && is.na(result)) {
      result <- dbGetQuery(con, query)
    } else {
      result <- result %>%
        bind_rows(dbGetQuery(con, query))
    }
  }

  return(result)
}


con_fm <-
  dbConnect(
    odbc::odbc(),
    .connection_string =
      paste0(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
        path_to_fieldmap
      )
  )

deadwood <- query_table(table = "Deadwood")
deadwood_diameters <-
  query_reltable(
    "Deadwood_diameters", related_table = deadwood, id = "IDDeadwood"
  )
deadwood_2eset <- query_table("Deadwood_2eSET")
deadwood_2eset_diameters <-
  query_reltable(
    "Deadwood_2eSET_Diameters", related_table = deadwood_2eset,
    id = "IDDeadwood_2eSET"
  )
deadwood_3eset <- query_table("Deadwood_3eSET")
if (nrow(deadwood_3eset) > 0) {
  deadwood_3eset_diameters <-
    query_reltable(
      "Deadwood_3eSET_Diameters", related_table = deadwood_3eset,
      id = "IDDeadwood_3eSET"
    )
} else {
  deadwood_3eset_diameters <-
    query_table("Deadwood_3eSET_Diameters", id = "IDDeadwood_3eSET") %>%
    filter(IDDeadwood_3eSET %in% deadwood_3eset$ID)
}

plotdetails_1986 <- query_table("Plotdetails_1986")
plotdetails_1eset <- query_table("Plotdetails_1eSet")
plotdetails_2eset <- query_table("Plotdetails_2eSet")
plotdetails_3eset <- query_table("Plotdetails_3eSet")

columns <- dbListFields(con_fm, "Plots")
columns <- columns[!grepl("^FM", columns)]
columns <- columns[!grepl("^FieldMap", columns)]
columns <- paste("t1.", columns, sep = "", collapse = ", ")
plots <-
  dbGetQuery(
    con_fm,
    sprintf(
      "SELECT %s FROM Plots t1
        WHERE t1.ID IN (101, 11000, 21000, 141100, 204, 1005, 2006);",
      columns
    )
  )
rm(columns)

q_alive_dead <- dbReadTable(con_fm, "qAliveDead")
q_branch_lenght_reduction <- dbReadTable(con_fm, "qBranchLenghtReduction")
q_brows_index <- dbReadTable(con_fm, "qBrowsIndex")
q_cover_herbs <- dbReadTable(con_fm, "qCoverHerbs")
q_crown_vol_redu <- dbReadTable(con_fm, "qCrownVolRedu")
q_dbh_class_5cm <- dbReadTable(con_fm, "qDBHClass_5cm")
qdecaystage <- dbReadTable(con_fm, "qdecaystage")
qdiameterclass <- dbReadTable(con_fm, "qdiameterclass")
qheightclass_regeneration <- dbReadTable(con_fm, "qHeightClass_regeneration")
qherbspecies240810 <-
  dbGetQuery(con_fm, "SELECT * FROM qHerbSpecies240810 WHERE ID IN (131, 161)")
qindshootcop <- dbReadTable(con_fm, "qIndShootCop")
qintactsnag <- dbReadTable(con_fm, "qIntactSnag")
qiufroheight <- dbReadTable(con_fm, "qiufroheight")
qiufroheight_shoots <- dbReadTable(con_fm, "qiufroheight_shoots")
qiufrosocialstatus <- dbReadTable(con_fm, "qiufrosocialstatus")
qiufrosocialstatus_shoots <- dbReadTable(con_fm, "qiufrosocialstatus_shoots")
qiufrovitality <- dbReadTable(con_fm, "qiufrovitality")
qiufrovitality_shoots <- dbReadTable(con_fm, "qiufrovitality_shoots")
qnumber_regeneration_classes <-
  dbReadTable(con_fm, "qnumber_regeneration_classes")
qplottype <- dbReadTable(con_fm, "qPlotType")
qspecies <-
  dbGetQuery(
    con_fm,
    "SELECT * FROM qspecies WHERE ID IN (6, 7, 16, 26, 28, 87, 131, 161)"
  )
qtotalcover <- dbReadTable(con_fm, "qtotalCover")
q_yes_no <- dbReadTable(con_fm, "qYesNo")

regeneration <- query_table("Regeneration")
regeneration_2eset <- query_table("Regeneration_2eSet")
regeneration_3eset <- query_table("Regeneration_3eSet")

heightclass <-
  query_reltable(
    "HeightClass", related_table = regeneration, id = "IDRegeneration"
  )
heightclass_2eset <-
  query_reltable(
    "HeightClass_2eSet", related_table = regeneration_2eset,
    id = "IDRegeneration_2eSet"
  )
if (nrow(regeneration_3eset) > 0) {
  heightclass_3eset <-
    query_reltable(
      "HeightClass_3eSet", related_table = regeneration_3eset,
      id = "IDRegeneration_3eSet"
    )
} else {
  heightclass_3eset <-
    query_table("HeightClass_3eSet", id = "IDRegeneration_3eSet")
}

regspecies <-
  query_reltable(
    "RegSpecies", related_table = regeneration, id = "IDRegeneration"
  )
regspecies_2eset <-
  query_reltable(
    "RegSpecies_2eSet", related_table = regeneration_2eset,
    id = "IDRegeneration_2eSet"
  )
if (nrow(regeneration_3eset) > 0) {
  regspecies_3eset <-
    query_reltable(
      "RegSpecies_3eSet", related_table = regeneration_3eset,
      id = "IDRegeneration_3eSet"
    )
} else {
  regspecies_3eset <-
    query_table("RegSpecies_3eSet", id = "IDRegeneration_3eSet")
}


trees_1986 <- query_table("Trees_1986", plot_id = "11000")
trees <- query_table("Trees", plot_id = "101, 21000", top_x = 10) %>%
  bind_rows(query_table("Trees", plot_id = "11000, 141100, 204, 1005, 2006"))
trees_2eset <-
  query_reltable("Trees_2eSET", related_table = trees, id = "Oldid") %>%
  bind_rows(
    query_table("Trees_2eSET", plot_id = "101, 21000", top_x = 10) %>%
      filter(is.na(.data$OldID))
  )
trees_3eset <-
  query_reltable("Trees_3eSET", related_table = trees_2eset, id = "OldId") %>%
  bind_rows(
    query_table("Trees_3eSET", plot_id = "101, 21000", top_x = 10) %>%
      filter(is.na(.data$OldID))
  )

shoots_1986 <-
  query_reltable("Shoots_1986", related_table = trees_1986, id = "IDTrees_1986")
shoots <- query_reltable("Shoots", related_table = trees, id = "IDTrees")
shoots_2eset <-
  query_reltable(
    "Shoots_2eSET", related_table = trees_2eset, id = "IDTrees_2eSET"
  )
shoots_3eset <-
  query_reltable(
    "Shoots_3eSET", related_table = trees_3eset, id = "IDTrees_3eSET"
  )


vegetation <- query_table("Vegetation", plot_id = "101, 21000", top_x = 10) %>%
  bind_rows(
    query_table("Vegetation", plot_id = "11000, 141100, 204, 1005, 2006")
  )
vegetation_2eset <-
  query_table("Vegetation_2eSet", plot_id = "101, 21000", top_x = 10) %>%
  bind_rows(
    query_table("Vegetation_2eSet", plot_id = "11000, 141100, 204, 1005, 2006")
  )
vegetation_3eset <-
  query_table("Vegetation_3eSet", plot_id = "101, 21000", top_x = 10) %>%
  bind_rows(
    query_table("Vegetation_3eSet", plot_id = "11000, 141100, 204, 1005, 2006")
  )

herblayer <-
  query_reltable(
    "Herblayer", related_table = vegetation, id = "IDVegetation"
  ) %>%
  filter(.data$Species %in% qherbspecies240810$ID)
herblayer_2eset <-
  query_reltable(
    "Herblayer_2eSet", related_table = vegetation_2eset,
    id = "IDVegetation_2eSet"
  ) %>%
  filter(.data$Species %in% qherbspecies240810$ID)
if (nrow(vegetation_3eset) > 0) {
  herblayer_3eset <-
    query_reltable(
      "Herblayer_3eSet", related_table = vegetation_3eset,
      id = "IDVegetation_3eSet"
    ) %>%
    filter(.data$Species %in% qherbspecies240810$ID)
} else {
  herblayer_3eset <-
    query_table("Herblayer_3eSet", id = "IDVegetation_3eSet") %>%
    filter(.data$Species %in% qherbspecies240810$ID)
}

dbDisconnect(con_fm)


library(RSQLite)

unlink("inst/example/database/mdb_bosres.sqlite")

packagedb <- dbConnect(SQLite(), "inst/example/database/mdb_bosres.sqlite")
dbWriteTable(packagedb, "Deadwood", deadwood)
dbWriteTable(packagedb, "Deadwood_2eSET", deadwood_2eset)
dbWriteTable(packagedb, "Deadwood_2eSET_Diameters", deadwood_2eset_diameters)
dbWriteTable(packagedb, "Deadwood_3eSET", deadwood_3eset)
dbWriteTable(packagedb, "Deadwood_3eSET_Diameters", deadwood_3eset_diameters)
dbWriteTable(packagedb, "Deadwood_Diameters", deadwood_diameters)
dbWriteTable(packagedb, "HeightClass", heightclass)
dbWriteTable(packagedb, "HeightClass_2eSet", heightclass_2eset)
dbWriteTable(packagedb, "HeightClass_3eSet", heightclass_3eset)
dbWriteTable(packagedb, "Herblayer", herblayer)
dbWriteTable(packagedb, "Herblayer_2eSet", herblayer_2eset)
dbWriteTable(packagedb, "Herblayer_3eSet", herblayer_3eset)
dbWriteTable(packagedb, "Plotdetails_1986", plotdetails_1986)
dbWriteTable(packagedb, "Plotdetails_1eSet", plotdetails_1eset)
dbWriteTable(packagedb, "Plotdetails_2eSet", plotdetails_2eset)
dbWriteTable(packagedb, "Plotdetails_3eSet", plotdetails_3eset)
dbWriteTable(packagedb, "Plots", plots)
dbWriteTable(packagedb, "qAliveDead", q_alive_dead)
dbWriteTable(packagedb, "qBranchLenghtReduction", q_branch_lenght_reduction)
dbWriteTable(packagedb, "qBrowsIndex", q_brows_index)
dbWriteTable(packagedb, "qCoverHerbs", q_cover_herbs)
dbWriteTable(packagedb, "qCrownVolRedu", q_crown_vol_redu)
dbWriteTable(packagedb, "qDBHClass_5cm", q_dbh_class_5cm)
dbWriteTable(packagedb, "qdecaystage", qdecaystage)
dbWriteTable(packagedb, "qdiameterclass", qdiameterclass)
dbWriteTable(packagedb, "qHeightClass_regeneration", qheightclass_regeneration)
dbWriteTable(packagedb, "qHerbSpecies240810", qherbspecies240810)
dbWriteTable(packagedb, "qIndShootCop", qindshootcop)
dbWriteTable(packagedb, "qIntactSnag", qintactsnag)
dbWriteTable(packagedb, "qiufroheight", qiufroheight)
dbWriteTable(packagedb, "qiufroheight_shoots", qiufroheight_shoots)
dbWriteTable(packagedb, "qiufrosocialstatus", qiufrosocialstatus)
dbWriteTable(packagedb, "qiufrosocialstatus_shoots", qiufrosocialstatus_shoots)
dbWriteTable(packagedb, "qiufrovitality", qiufrovitality)
dbWriteTable(packagedb, "qiufrovitality_shoots", qiufrovitality_shoots)
dbWriteTable(
  packagedb, "qnumber_regeneration_classes", qnumber_regeneration_classes
)
dbWriteTable(packagedb, "qPlotType", qplottype)
dbWriteTable(packagedb, "qspecies", qspecies)
dbWriteTable(packagedb, "qtotalCover", qtotalcover)
dbWriteTable(packagedb, "qYesNo", q_yes_no)
dbWriteTable(packagedb, "Regeneration", regeneration)
dbWriteTable(packagedb, "Regeneration_2eSet", regeneration_2eset)
dbWriteTable(packagedb, "Regeneration_3eSet", regeneration_3eset)
dbWriteTable(packagedb, "RegSpecies", regspecies)
dbWriteTable(packagedb, "RegSpecies_2eSet", regspecies_2eset)
dbWriteTable(packagedb, "RegSpecies_3eSet", regspecies_3eset)
dbWriteTable(packagedb, "Shoots", shoots)
dbWriteTable(packagedb, "Shoots_1986", shoots_1986)
dbWriteTable(packagedb, "Shoots_2eSET", shoots_2eset)
dbWriteTable(packagedb, "Shoots_3eSET", shoots_3eset)
dbWriteTable(packagedb, "Trees", trees)
dbWriteTable(packagedb, "Trees_1986", trees_1986)
dbWriteTable(packagedb, "Trees_2eSET", trees_2eset)
dbWriteTable(packagedb, "Trees_3eSET", trees_3eset)
dbWriteTable(packagedb, "Vegetation", vegetation)
dbWriteTable(packagedb, "Vegetation_2eSet", vegetation_2eset)
dbWriteTable(packagedb, "Vegetation_3eSet", vegetation_3eset)

dbDisconnect(packagedb)
