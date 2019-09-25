library(forrescalc)

data_dendro <-
  load_data_dendrometry(
    database = "C:/R/bosreservatendb/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb"
  )

dendro <- calculate_dendrometry(data_dendro)

save_results_git(
  results = dendro,
  repo_path = "C:/R/bosreservaten/forresdat"
)
save_results_access(
  results = dendro,
  database = "C:/R/bosreservatendb/testdb.accdb"
)

calculate_regeneration(...)
calculate_vegetation(...)
