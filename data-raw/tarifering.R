# This script downloads data on 'tarifering' and saves them in the package
# for use during calculations in calc_variables_tree_level()
# !!! rebuild the package to use the newly generated tables in your calculations
# (Set environment variable db_externe_data, see main script for explanation)

library(DBI)
library(readr)

db_externe_data <- Sys.getenv("db_externe_data")

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

query_coef_convert_perimeter <-
  "SELECT IDTreeSp AS species, A, B
  FROM tblCoefOmzetOmtrek"

con <- forrescalc:::connect_to_database(db_externe_data)

tariffs2entr <- dbGetQuery(con, query_tariffs2entries, stringsAsFactors = TRUE)
tariffs1entr <- dbGetQuery(con, query_tariffs1entry, stringsAsFactors = TRUE)
tariffs1entr_crown <-
  dbGetQuery(con, query_tariffs1entry_crown, stringsAsFactors = TRUE)
convert_perimeter <-
  dbGetQuery(con, query_coef_convert_perimeter, stringsAsFactors = TRUE)

dbDisconnect(con)

colnames(convert_perimeter) <- tolower(colnames(convert_perimeter))

write_csv2(tariffs2entr, "inst/extdata/tariffs2entries.csv")
write_csv2(tariffs1entr, "inst/extdata/tariffs1entry.csv")
write_csv2(tariffs1entr_crown, "inst/extdata/tariffs1entry_crown.csv")
write_csv2(convert_perimeter, "inst/extdata/convert_perimeter.csv")
