library(forrescalc)

data_dendro <-
  load_data_dendrometry(
    database = "C:/R/bosreservatendb/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb"
  )
data_deadwood <-
  load_data_deadwood(
    database = "C:/R/bosreservatendb/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb"
  )

dendro <- calculate_dendrometry(data_dendro, data_deadwood)

save_results_git(
  results = dendro,
  repo_path = "C:/R/bosreservaten/forresdat"
)
save_results_access(
  results = dendro,
  database = "C:/R/bosreservatendb/testdb.accdb"
)

data_regeneration <-
  load_data_regeneration(
    database = "C:/R/bosreservatendb/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb"
  )

regeneration <- calculate_regeneration(data_regeneration)

save_results_git(
  results = regeneration,
  repo_path = "C:/R/bosreservaten/forresdat"
)
save_results_access(
  results = regeneration,
  database = "C:/R/bosreservatendb/testdb.accdb"
)

calculate_vegetation(...)
