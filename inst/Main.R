library(tidyverse)
library(forrescalc)

path_to_fieldmap <- "C:/R/bosreservatendb/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb"
path_to_git_forresdat <- "C:/R/bosreservatendb/forresdat"
path_to_height_models <- "C:/Hoogtemodellen"

#only when q-tables in Fieldmap have changed (and only mention the changed table)
from_access_to_git(
  database = path_to_fieldmap,
  tables = c("qAliveDead", "qSpecies", "qHeightClass_regeneration",
             "qnumber_regeneration_classes", "qdecaystage"),
  repo_path = path_to_git_forresdat
)

plotinfo <- load_plotinfo(database = path_to_fieldmap)
save_results_git(
  results = list(plotinfo = plotinfo),
  repo_path = path_to_git_forresdat
)

data_dendro <-
  load_data_dendrometry(
    database = path_to_fieldmap
  )
data_deadwood <-
  load_data_deadwood(
    database = path_to_fieldmap
  )
data_shoots <-
  load_data_shoots(
    database = path_to_fieldmap
  )
data_stems  <- compose_stem_data(data_dendro, data_shoots)
height_model <- load_height_models(path_to_height_models)

dendro <- calculate_dendrometry(data_dendro, data_deadwood, data_stems,
                                height_model, plotinfo)

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
data_herblayer <-
  load_data_herblayer(
    database = path_to_fieldmap
  )

vegetation <- calculate_vegetation(data_vegetation, data_herblayer)

save_results_git(
  results = vegetation,
  repo_path = path_to_git_forresdat
)
