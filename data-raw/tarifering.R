# This script downloads data on 'tarifering' and saves them in the package
# for use during calculations in calc_variables_tree_level()
# !!! rebuild the package to use the newly generated tables in your calculations

library(RODBC)
library(readr)

dbExterneData <- "C:\3BR\DataVerwerkingBR\Data\BR_ExterneData.accdb"

query_tarieven2ing <-
  "SELECT tgbs.ID AS species
  , tgbs.Value1 AS name_nl
  , t2.Tarief As tarief
  , t2.groepNaam AS groepnaam
  , t2.a
  , t2.b
  , t2.c
  , t2.d
  , t2.e
  , t2.f
  , t2.g
  , t2.Formule_type AS formule_type
  FROM tblTariefgroepBoomsoort tgbs
    INNER JOIN tblTarieven_2ing t2 ON tgbs.TariefID = t2.groepID;"

query_tarieven1ing <-
  "SELECT tgbs.ID AS species
  , tgbs.Value1 AS name_nl
  , tgbs.TariefID AS tarief_id
  , t1.groepNaam AS groepnaam
  , t1.a
  , t1.b
  , t1.c
  , t1.d
  , t1.Tarief AS tarief
  FROM tblTariefgroepBoomsoort tgbs
    LEFT JOIN tblTarieven_1ing t1 ON tgbs.TariefID = t1.groepID;"

query_tarieven1ing_crown <-
  "SELECT tgbs.ID AS species
  , tgbs.Value1 AS name_nl
  , tgbs.TariefID AS tarief_id
  , t1k.groepNaam
  , t1k.a
  , t1k.b
  , t1k.c
  , t1k.d
  , t1k.Tarief AS tarief
  FROM tblTariefgroepBoomsoort tgbs
    LEFT JOIN tblTarieven_1ingKroon t1k ON tgbs.TariefID = t1k.groepID;"

con <- odbcConnectAccess2007(dbExterneData)

tarieven2ing <- sqlQuery(con, query_tarieven2ing, stringsAsFactors = TRUE)
tarieven1ing <- sqlQuery(con, query_tarieven1ing, stringsAsFactors = TRUE)
tarieven1ing_crown <-
  sqlQuery(con, query_tarieven1ing_crown, stringsAsFactors = TRUE)

odbcClose(con)

write_csv2(tarieven2ing, "inst/extdata/tarieven2ing.csv")
write_csv2(tarieven1ing, "inst/extdata/tarieven1ing.csv")
write_csv2(tarieven1ing_crown, "inst/extdata/tarieven1ing_crown.csv")
