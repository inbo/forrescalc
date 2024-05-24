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

dbDisconnect(con_testdb)
