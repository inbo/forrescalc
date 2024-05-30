# This file generates (or updates) a minimal test dataset that is added to
# the package for testing (and examples with output for check functions).
# It should be updated after structural changes are made in the FM database
# and the example dataset (of which it is derived) is updated.

library(DBI)
library(dplyr)
library(RSQLite)

unlink("inst/example/testdb/mdb_bosres_test.sqlite")

con_exampledb <- dbConnect(SQLite(), "inst/example/database/mdb_bosres.sqlite")
con_testdb <- dbConnect(SQLite(), "inst/example/testdb/mdb_bosres_test.sqlite")

sqliteCopyDatabase(con_exampledb, con_testdb)

dbDisconnect(con_exampledb)

deadwood <- dbReadTable(con_testdb, "Deadwood")
deadwood[deadwood$ID == 11587, "IntactFragment"] <- NA
deadwood[deadwood$ID == 11626, "IntactFragment"] <- 40
deadwood[deadwood$ID == 11629, "IntactFragment"] <- 30
deadwood[deadwood$ID == 21595, "IntactFragment"] <- 10
deadwood[deadwood$ID == 1, "IntactFragment"] <- 10 # (not wrong)
deadwood[deadwood$ID == 11587, "AliveDead"] <- 11
deadwood[deadwood$ID == 11587, "DecayStage"] <- NA
deadwood[deadwood$ID == 11626, "DecayStage"] <- 17
deadwood[deadwood$ID == 11629, "DecayStage"] <- 16
dbWriteTable(con_testdb, "Deadwood", deadwood, overwrite = TRUE)

deadwood_diameters <- dbReadTable(con_testdb, "Deadwood_Diameters")
deadwood_diameters[deadwood_diameters$IDDeadwood == 11587, "Diameter_mm"] <- 10
dbWriteTable(
  con_testdb, "Deadwood_Diameters", deadwood_diameters, overwrite = TRUE
)

herblayer <- dbReadTable(con_testdb, "Herblayer")
herblayer[herblayer$ID == 119, "Coverage"] <- NA
herblayer[herblayer$ID == 120, "Coverage"] <- 19
herblayer[herblayer$ID == 119, "Species"] <-
  herblayer[herblayer$ID == 120, "Species"]
herblayer[herblayer$ID == 119, "BrowseIndex"] <- NA
herblayer[herblayer$ID == 120, "BrowseIndex"] <- 130
dbWriteTable(con_testdb, "Herblayer", herblayer, overwrite = TRUE)

plotdetails_1eset <-
  data.frame(
    IDPlots = c(20, 30, 40, 50, 60, 70), ID = 1,
    ForestReserve = c(rep(NA, 4), rep("Everzwijnbad", 2)),
    Date_Dendro_1eSet = c(rep(NA, 4), 1038700800, 1138700800)
  )
dbWriteTable(con_testdb, "Plotdetails_1eSet", plotdetails_1eset, append = TRUE)

plots <-
  data.frame(
    ID = c(20, 30, 40, 50, 60, 70), Plottype = c(20, 30, NA, 80, 20, 20)
  )
dbWriteTable(con_testdb, "Plots", plots, append = TRUE)

regeneration <- dbReadTable(con_testdb, "Regeneration")
regeneration[regeneration$IDPlots == 101, "Fieldteam"] <- NA
regeneration[regeneration$IDPlots == 101, "Date"] <- NA
regeneration[regeneration$ID == 155513, "Date"] <- 995088000
dbWriteTable(con_testdb, "Regeneration", regeneration, overwrite = TRUE)

heightclass <-              #same HeightClass as ID 142
  data.frame(IDPlots = 101, IDRegeneration = 1, ID = 143, HeightClass = 3000)
dbWriteTable(con_testdb, "HeightClass", heightclass, append = TRUE)

regspecies <-
  data.frame(
    IDPlots = 101, IDRegeneration = 1, IDHeightClass = 143, ID = 150,
    Species = 39, NumberClass = 1
  )
dbWriteTable(con_testdb, "RegSpecies", regspecies, append = TRUE)

regspecies_3eset <- dbReadTable(con_testdb, "RegSpecies_3eSet")
regspecies_3eset[
  regspecies_3eset$IDHeightClass_3eSet == 1, "GameDamage_number"] <- 70
regspecies_3eset[
  regspecies_3eset$IDHeightClass_3eSet == 3, "GameDamage_number"] <- 20
dbWriteTable(con_testdb, "RegSpecies_3eSet", regspecies_3eset, overwrite = TRUE)

vegetation <-
  data.frame(
    IDPlots = c(rep(20, 3), rep(60, 2)), ID = 1:5,
    Total_moss_cover = c(NA, 15, 20, 10, 10),
    Total_herb_cover = c(NA, 15, 20, 10, 10),
    Total_shrub_cover = c(NA, 15, 20, 10, 10),
    Total_tree_cover = c(NA, 15, 20, 10, 10),
    Total_waterlayer_cover = c(NA, 15, 20, 10, 10),
    Total_SoildisturbanceGame = c(NA, 15, 20, 10, 10),
    Date = c(rep(NA, 3), 1022716800, 1122716800)
  )
dbWriteTable(con_testdb, "Vegetation", vegetation, append = TRUE)

shoots <-
  data.frame(
    IDPlots = c(101, 101, 21000, 21000),
    IDTrees = c(11559, 11557, 55, 55),
    ID = c(1, 3, 2, 3),
    XTrees = c(-4.767, -4.767, 153406.01, 153406.01),
    YTrees = c(3.229, 3.229, 161257.186, 161257.186),
    DBH_mm = c(2001, 1, NA, NA), Height_m = c(1, 55, NA, NA),
    IntactSnag = c(12, 11, 10, NA),
    DecayStage_Shoots = c(17, 11, 16, NA),
    IUFROHght = c(50, 40, 10, NA),
    IUFROVital = c(50, 40, 20, NA),
    IUFROSocia = c(50, 40, 30, NA)
  )
dbWriteTable(con_testdb, "Shoots", shoots, append = TRUE)

dbDisconnect(con_testdb)
