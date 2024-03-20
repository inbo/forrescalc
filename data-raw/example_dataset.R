# This file generates (or updates) a minimal example dataset that is added to
# the package to provide working examples and for testing reasons.
# It should be updated after structural changes are made in the FM database.

path_to_fieldmap <-
  "C:/R/bosreservatendb/MDB_BOSRES_selectieEls/FieldMapData_dB_Els_deel2.accdb"

library(DBI)
library(dplyr)

query_table <-
  function(
    table, con = con_FM, id = "ID",
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

query_reltable <- function(table, con = con_FM, related_table, id = "ID") {
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


con_FM <-
  dbConnect(
    odbc::odbc(),
    .connection_string =
      paste0(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
        path_to_fieldmap
      )
  )

Deadwood <- query_table(table = "Deadwood")
Deadwood_Diameters <-
  query_reltable(
    "Deadwood_diameters", related_table = Deadwood, id = "IDDeadwood"
  )
Deadwood_2eSET <- query_table("Deadwood_2eSET")
Deadwood_2eSET_Diameters <-
  query_reltable(
    "Deadwood_2eSET_Diameters", related_table = Deadwood_2eSET,
    id = "IDDeadwood_2eSET"
  )
Deadwood_3eSET <- query_table("Deadwood_3eSET")
if (nrow(Deadwood_3eSET) > 0) {
  Deadwood_3eSET_Diameters <-
    query_reltable(
      "Deadwood_3eSET_Diameters", related_table = Deadwood_3eSET,
      id = "IDDeadwood_3eSET"
    )
} else {
  Deadwood_3eSET_Diameters <-
    query_table("Deadwood_3eSET_Diameters", id = "IDDeadwood_3eSET") %>%
    filter(IDDeadwood_3eSET %in% Deadwood_3eSET$ID)
}

Plotdetails_1986 <- query_table("Plotdetails_1986")
Plotdetails_1eSet <- query_table("Plotdetails_1eSet")
Plotdetails_2eSet <- query_table("Plotdetails_2eSet")
Plotdetails_3eSet <- query_table("Plotdetails_3eSet")

columns <- dbListFields(con_FM, "Plots")
columns <- columns[!grepl("^FM", columns)]
columns <- columns[!grepl("^FieldMap", columns)]
columns <- paste("t1.", columns, sep = "", collapse = ", ")
Plots <-
  dbGetQuery(
    con_FM,
    sprintf(
      "SELECT %s FROM Plots t1
        WHERE t1.ID IN (101, 11000, 21000, 141100, 204, 1005, 2006);",
      columns
    )
  )
rm(columns)

qAliveDead <- dbReadTable(con_FM, "qAliveDead")
qBranchLenghtReduction <- dbReadTable(con_FM, "qBranchLenghtReduction")
qBrowsIndex <- dbReadTable(con_FM, "qBrowsIndex")
qCoverHerbs <- dbReadTable(con_FM, "qCoverHerbs")
qCrownVolRedu <- dbReadTable(con_FM, "qCrownVolRedu")
qDBHClass_5cm <- dbReadTable(con_FM, "qDBHClass_5cm")
qdecaystage <- dbReadTable(con_FM, "qdecaystage")
qdiameterclass <- dbReadTable(con_FM, "qdiameterclass")
qHeightClass_regeneration <- dbReadTable(con_FM, "qHeightClass_regeneration")
qHerbSpecies240810 <-
  dbGetQuery(con_FM, "SELECT * FROM qHerbSpecies240810 WHERE ID IN (131, 161)")
qIndShootCop <- dbReadTable(con_FM, "qIndShootCop")
qIntactSnag <- dbReadTable(con_FM, "qIntactSnag")
qiufroheight <- dbReadTable(con_FM, "qiufroheight")
qiufroheight_shoots <- dbReadTable(con_FM, "qiufroheight_shoots")
qiufrosocialstatus <- dbReadTable(con_FM, "qiufrosocialstatus")
qiufrosocialstatus_shoots <- dbReadTable(con_FM, "qiufrosocialstatus_shoots")
qiufrovitality <- dbReadTable(con_FM, "qiufrovitality")
qiufrovitality_shoots <- dbReadTable(con_FM, "qiufrovitality_shoots")
qnumber_regeneration_classes <-
  dbReadTable(con_FM, "qnumber_regeneration_classes")
qPlotType <- dbReadTable(con_FM, "qPlotType")
qspecies <-
  dbGetQuery(
    con_FM,
    "SELECT * FROM qspecies WHERE ID IN (6, 7, 16, 26, 28, 87, 131, 161)"
  )
qtotalCover <- dbReadTable(con_FM, "qtotalCover")
qYesNo <- dbReadTable(con_FM, "qYesNo")

Regeneration <- query_table("Regeneration")
Regeneration_2eSet <- query_table("Regeneration_2eSet")
Regeneration_3eSet <- query_table("Regeneration_3eSet")

HeightClass <-
  query_reltable(
    "HeightClass", related_table = Regeneration, id = "IDRegeneration"
  )
HeightClass_2eSet <-
  query_reltable(
    "HeightClass_2eSet", related_table = Regeneration_2eSet,
    id = "IDRegeneration_2eSet"
  )
if (nrow(Regeneration_3eSet) > 0) {
  HeightClass_3eSet <-
    query_reltable(
      "HeightClass_3eSet", related_table = Regeneration_3eSet,
      id = "IDRegeneration_3eSet"
    )
} else {
  HeightClass_3eSet <-
    query_table("HeightClass_3eSet", id = "IDRegeneration_3eSet")
}

RegSpecies <-
  query_reltable(
    "RegSpecies", related_table = Regeneration, id = "IDRegeneration"
  )
RegSpecies_2eSet <-
  query_reltable(
    "RegSpecies_2eSet", related_table = Regeneration_2eSet,
    id = "IDRegeneration_2eSet"
  )
if (nrow(Regeneration_3eSet) > 0) {
  RegSpecies_3eSet <-
    query_reltable(
      "RegSpecies_3eSet", related_table = Regeneration_3eSet,
      id = "IDRegeneration_3eSet"
    )
} else {
  RegSpecies_3eSet <-
    query_table("RegSpecies_3eSet", id = "IDRegeneration_3eSet")
}


Trees_1986 <- query_table("Trees_1986", plot_id = "11000")
Trees <- query_table("Trees", plot_id = "101, 21000", top_x = 10) %>%
  bind_rows(query_table("Trees", plot_id = "11000, 141100, 204, 1005, 2006"))
Trees_2eSET <-
  query_reltable("Trees_2eSET", related_table = Trees, id = "Oldid") %>%
  bind_rows(
    query_table("Trees_2eSET", plot_id = "101, 21000", top_x = 10) %>%
      filter(is.na(.data$OldID))
  )
Trees_3eSET <-
  query_reltable("Trees_3eSET", related_table = Trees_2eSET, id = "OldId") %>%
  bind_rows(
    query_table("Trees_3eSET", plot_id = "101, 21000", top_x = 10) %>%
      filter(is.na(.data$OldID))
  )

Shoots_1986 <-
  query_reltable("Shoots_1986", related_table = Trees_1986, id = "IDTrees_1986")
Shoots <- query_reltable("Shoots", related_table = Trees, id = "IDTrees")
Shoots_2eSET <-
  query_reltable(
    "Shoots_2eSET", related_table = Trees_2eSET, id = "IDTrees_2eSET"
  )
Shoots_3eSET <-
  query_reltable(
    "Shoots_3eSET", related_table = Trees_3eSET, id = "IDTrees_3eSET"
  )


Vegetation <- query_table("Vegetation", plot_id = "101, 21000", top_x = 10) %>%
  bind_rows(
    query_table("Vegetation", plot_id = "11000, 141100, 204, 1005, 2006")
  )
Vegetation_2eSet <-
  query_table("Vegetation_2eSet", plot_id = "101, 21000", top_x = 10) %>%
  bind_rows(
    query_table("Vegetation_2eSet", plot_id = "11000, 141100, 204, 1005, 2006")
  )
Vegetation_3eSet <-
  query_table("Vegetation_3eSet", plot_id = "101, 21000", top_x = 10) %>%
  bind_rows(
    query_table("Vegetation_3eSet", plot_id = "11000, 141100, 204, 1005, 2006")
  )

Herblayer <-
  query_reltable(
    "Herblayer", related_table = Vegetation, id = "IDVegetation"
  ) %>%
  filter(.data$Species %in% qHerbSpecies240810$ID)
Herblayer_2eSet <-
  query_reltable(
    "Herblayer_2eSet", related_table = Vegetation_2eSet,
    id = "IDVegetation_2eSet"
  ) %>%
  filter(.data$Species %in% qHerbSpecies240810$ID)
if (nrow(Vegetation_3eSet) > 0) {
  Herblayer_3eSet <-
    query_reltable(
      "Herblayer_3eSet", related_table = Vegetation_3eSet,
      id = "IDVegetation_3eSet"
    ) %>%
    filter(.data$Species %in% qHerbSpecies240810$ID)
} else {
  Herblayer_3eSet <-
    query_table("Herblayer_3eSet", id = "IDVegetation_3eSet") %>%
    filter(.data$Species %in% qHerbSpecies240810$ID)
}

dbDisconnect(con_FM)


library(RSQLite)

unlink("inst/example/database/mdb_bosres.sqlite")

packagedb <- dbConnect(SQLite(), "inst/example/database/mdb_bosres.sqlite")
dbWriteTable(packagedb, "Deadwood", Deadwood)
dbWriteTable(packagedb, "Deadwood_2eSET", Deadwood_2eSET)
dbWriteTable(packagedb, "Deadwood_2eSET_Diameters", Deadwood_2eSET_Diameters)
dbWriteTable(packagedb, "Deadwood_3eSET", Deadwood_3eSET)
dbWriteTable(packagedb, "Deadwood_3eSET_Diameters", Deadwood_3eSET_Diameters)
dbWriteTable(packagedb, "Deadwood_Diameters", Deadwood_Diameters)
dbWriteTable(packagedb, "HeightClass", HeightClass)
dbWriteTable(packagedb, "HeightClass_2eSet", HeightClass_2eSet)
dbWriteTable(packagedb, "HeightClass_3eSet", HeightClass_3eSet)
dbWriteTable(packagedb, "Herblayer", Herblayer)
dbWriteTable(packagedb, "Herblayer_2eSet", Herblayer_2eSet)
dbWriteTable(packagedb, "Herblayer_3eSet", Herblayer_3eSet)
dbWriteTable(packagedb, "Plotdetails_1986", Plotdetails_1986)
dbWriteTable(packagedb, "Plotdetails_1eSet", Plotdetails_1eSet)
dbWriteTable(packagedb, "Plotdetails_2eSet", Plotdetails_2eSet)
dbWriteTable(packagedb, "Plotdetails_3eSet", Plotdetails_3eSet)
dbWriteTable(packagedb, "Plots", Plots)
dbWriteTable(packagedb, "qAliveDead", qAliveDead)
dbWriteTable(packagedb, "qBranchLenghtReduction", qBranchLenghtReduction)
dbWriteTable(packagedb, "qBrowsIndex", qBrowsIndex)
dbWriteTable(packagedb, "qCoverHerbs", qCoverHerbs)
dbWriteTable(packagedb, "qCrownVolRedu", qCrownVolRedu)
dbWriteTable(packagedb, "qDBHClass_5cm", qDBHClass_5cm)
dbWriteTable(packagedb, "qdecaystage", qdecaystage)
dbWriteTable(packagedb, "qdiameterclass", qdiameterclass)
dbWriteTable(packagedb, "qHeightClass_regeneration", qHeightClass_regeneration)
dbWriteTable(packagedb, "qHerbSpecies240810", qHerbSpecies240810)
dbWriteTable(packagedb, "qIndShootCop", qIndShootCop)
dbWriteTable(packagedb, "qIntactSnag", qIntactSnag)
dbWriteTable(packagedb, "qiufroheight", qiufroheight)
dbWriteTable(packagedb, "qiufroheight_shoots", qiufroheight_shoots)
dbWriteTable(packagedb, "qiufrosocialstatus", qiufrosocialstatus)
dbWriteTable(packagedb, "qiufrosocialstatus_shoots", qiufrosocialstatus_shoots)
dbWriteTable(packagedb, "qiufrovitality", qiufrovitality)
dbWriteTable(packagedb, "qiufrovitality_shoots", qiufrovitality_shoots)
dbWriteTable(
  packagedb, "qnumber_regeneration_classes", qnumber_regeneration_classes
)
dbWriteTable(packagedb, "qPlotType", qPlotType)
dbWriteTable(packagedb, "qspecies", qspecies)
dbWriteTable(packagedb, "qtotalCover", qtotalCover)
dbWriteTable(packagedb, "qYesNo", qYesNo)
dbWriteTable(packagedb, "Regeneration", Regeneration)
dbWriteTable(packagedb, "Regeneration_2eSet", Regeneration_2eSet)
dbWriteTable(packagedb, "Regeneration_3eSet", Regeneration_3eSet)
dbWriteTable(packagedb, "RegSpecies", RegSpecies)
dbWriteTable(packagedb, "RegSpecies_2eSet", RegSpecies_2eSet)
dbWriteTable(packagedb, "RegSpecies_3eSet", RegSpecies_3eSet)
dbWriteTable(packagedb, "Shoots", Shoots)
dbWriteTable(packagedb, "Shoots_1986", Shoots_1986)
dbWriteTable(packagedb, "Shoots_2eSET", Shoots_2eSET)
dbWriteTable(packagedb, "Shoots_3eSET", Shoots_3eSET)
dbWriteTable(packagedb, "Trees", Trees)
dbWriteTable(packagedb, "Trees_1986", Trees_1986)
dbWriteTable(packagedb, "Trees_2eSET", Trees_2eSET)
dbWriteTable(packagedb, "Trees_3eSET", Trees_3eSET)
dbWriteTable(packagedb, "Vegetation", Vegetation)
dbWriteTable(packagedb, "Vegetation_2eSet", Vegetation_2eSet)
dbWriteTable(packagedb, "Vegetation_3eSet", Vegetation_3eSet)

dbDisconnect(packagedb)
