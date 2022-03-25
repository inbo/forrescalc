# This script generates an overview of all variables containing class data,
# containing their name, the name of the variables that represent min and max,
# and the transformation that should be used to deal with these data.
# In case of coverage, transformation is based on the class widths of the most
# abundant coverage classes
# (This table is used in create_statistics().)

library(readr)

class_data <-
  data.frame(
    var_name =
      c("number_of_regeneration", "moss_cover", "herb_cover", "shrub_cover",
        "tree_cover", "waterlayer_cover", "soildisturbance_game_cover"),
    var_min =
      c("min_number_of_regeneration", "moss_cover_min", "herb_cover_min",
        "shrub_cover_min", "tree_cover_min", "waterlayer_cover_min",
        "soildisturbance_game_cover_min"),
    var_max =
      c("max_number_of_regeneration", "moss_cover_max", "herb_cover_max",
        "shrub_cover_max", "tree_cover_max", "waterlayer_cover_max",
        "soildisturbance_game_cover_max"),
    preferred_transformation = c("log", "log", NA, "log", NA, "log", "log")
  )

write_csv2(class_data, "inst/extdata/class_data.csv")
