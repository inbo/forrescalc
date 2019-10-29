library(forrescalc)

path_to_fieldmap <- "C:/R/bosreservatendb/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb"
path_to_git_forresdat <- "C:/R/bosreservatendb/forresdat"

#only when q-tables in Fieldmap have changed (and only mention the changed table)
from_access_to_git(
  database = path_to_fieldmap,
  tables = c("qAliveDead", "qSpecies", "qHeightClass_regenaration", "qnumber_regeneration_classes", "qdecaystage"),
  repo_path = path_to_git_forresdat
)
#evt. ook gegevens over plot toevoegen?  Moeten eigenlijk telkens alle kolommen overgenomen worden, of toch beter met specifieke queries werken?

data_dendro <-
  load_data_dendrometry(
    database = path_to_fieldmap
  )
data_deadwood <-
  load_data_deadwood(
    database = path_to_fieldmap
  )

dendro <- calculate_dendrometry(data_dendro, data_deadwood)

save_results_git(
  results = dendro,
  repo_path = path_to_git_forresdat
)

data_regeneration <-
  load_data_regeneration(
    database = path_to_fieldmap
  )

regeneration <- calculate_regeneration(data_regeneration)

save_results_git(
  results = regeneration,
  repo_path = path_to_git_forresdat
)

data_vegetation <-
  load_data_vegetation(
    database = path_to_fieldmap
  )
calculate_vegetation(...)
