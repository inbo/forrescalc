# This script downloads data on 'tarifering' and saves them in the package
# for use during calculations in calc_variables_tree_level()
# !!! rebuild the package to use the newly generated tables in your calculations

library(RODBC)
library(readr)

db_externe_data <- "C:/3BR/1_DataVerwerkingBR/Data/BR_ExterneData.accdb"

query_tariffs2entries <-
  "SELECT tgbs.ID AS species
  , tgbs.Value1 AS name_nl
  , tgbs.TariefID AS tariff_id
  , t2.groepNaam AS tariff_group
  , t2.Tarief As source
  , t2.a
  , t2.b
  , t2.c
  , t2.d
  , t2.e
  , t2.f
  , t2.g
  , t2.Formule_type AS formula
  FROM tblTariefgroepBoomsoort tgbs
    INNER JOIN tblTarieven_2ing t2 ON tgbs.TariefID = t2.groepID;"

query_tariffs1entry <-
  "SELECT tgbs.ID AS species
  , tgbs.Value1 AS name_nl
  , tgbs.TariefID AS tariff_id
  , t1.groepNaam AS tariff_group
  , t1.Tarief As source
  , t1.a
  , t1.b
  , t1.c
  , t1.d
  FROM tblTariefgroepBoomsoort tgbs
    LEFT JOIN tblTarieven_1ing t1 ON tgbs.TariefID = t1.groepID;"

query_tariffs1entry_crown <-
  "SELECT tgbs.ID AS species
  , tgbs.Value1 AS name_nl
  , tgbs.TariefID AS tariff_id
  , t1k.groepNaam AS tariff_group
  , t1k.Tarief As source
  , t1k.a
  , t1k.b
  , t1k.c
  , t1k.d
  FROM tblTariefgroepBoomsoort tgbs
    LEFT JOIN tblTarieven_1ingKroon t1k ON tgbs.TariefID = t1k.groepID;"

con <- odbcConnectAccess2007(db_externe_data)

tariffs2entr <- sqlQuery(con, query_tariffs2entries, stringsAsFactors = TRUE)
tariffs1entr <- sqlQuery(con, query_tariffs1entry, stringsAsFactors = TRUE)
tariffs1entr_crown <-
  sqlQuery(con, query_tariffs1entry_crown, stringsAsFactors = TRUE)

odbcClose(con)

write_csv2(tariffs2entr, "inst/extdata/tariffs2entries.csv")
write_csv2(tariffs1entr, "inst/extdata/tariffs1entry.csv")
write_csv2(tariffs1entr_crown, "inst/extdata/tariffs1entry_crown.csv")
