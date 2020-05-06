library(tidyverse)
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
  ) %>%
  filter(plot_id != 11000)  # in KV Kersselaerspleyn (plot 11000) no lying deadwood is meausured
data_shoots <-
  load_data_shoots(
    database = path_to_fieldmap
  )
data_stems  <- compose_stem_data(data_dendro, data_shoots)

dendro <- calculate_dendrometry(data_dendro, data_deadwood, data_stems)

# in KV Kersselaerspleyn (plot 11000) no lying deadwood is meausured
# if plotid 11000 in data, replace (lying) deadwood results for plot_id = 11000 by "NA"
dendro[["dendro_by_plot"]] <- dendro[["dendro_by_plot"]] %>%
  mutate(
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha),
    volume_deadwood_m3_ha  =
      ifelse(plot_id == 11000 & is.na(volume_log_m3_ha), NA, volume_log_m3_ha)
  )
dendro[["dendro_by_plot_species"]] <- dendro[["dendro_by_plot_species"]] %>%
  mutate(
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha),
    volume_deadwood_m3_ha  =
      ifelse(plot_id == 11000 & is.na(volume_log_m3_ha), NA, volume_log_m3_ha)
  )
dendro[["dendro_by_diam_plot"]] <- dendro[["dendro_by_diam_plot"]] %>%
  mutate(
    log_number_ha  =
      ifelse(plot_id == 11000 & log_number_ha == 0, NA, log_number_ha),
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha)
  )
dendro[["dendro_by_diam_plot_species"]] <- dendro[["dendro_by_diam_plot_species"]] %>%
  mutate(
    log_number_ha  =
      ifelse(plot_id == 11000 & log_number_ha == 0, NA, log_number_ha),
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha)
  )


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

vegetation <- calculate_vegetation(data_vegetation)

save_results_git(
  results = vegetation,
  repo_path = path_to_git_forresdat
)
