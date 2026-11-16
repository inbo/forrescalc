# forrescalc 0.0.2

__Breaking changes__

* function `calc_dendro_plot_species()`: removed variable `vol_deadw_m3_ha`,
  which previously combined standing and lying deadwood volumes,
  to avoid ambiguity about the included components.
* functions `load_data_dendrometry()` and `load_plotinfo()`:
  rename `dbh_min_a3` to `dbh_min_a3_alive` and `dbh_min_a4` to
  `dbh_min_a4_alive`, to be more in line with `dbh_min_a3_dead` and to avoid
  confusion.
* functions `load_data_vegetation()` and `load_data_regeneration()`:
  rename variable `year_main_survey` (loaded from `Fieldmap`) to `year_recorded`
  to avoid confusion.
* functions `load_data_vegetation()` and `calc_veg_plot()`:
  rename (calculated) variable `year_main_survey` to `year` to avoid confusion
  and to be more in line with the function `load_data_regeneration()`.

__Bug fixes__

* functions `calc_reg_plot_height(_species)()` and 
  `calc_reg_core_area_height_species()`: fixed incorrect zero values for records
  where game impact was not recorded. 
* minor corrections of check functions (a.o. if no survey, no need to test 
  missing values)

__Enhancements__

* functions `read_forresdat()`, `read_forresdat_table()` and
  `from_forresdat_to_access()`: extra arguments `git_ref_type` and
  `git_reference` that allow to read branches and commits from `forresdat`
  (not only releases)
* minor bug fixes and documentation corrections
* check functions made a bit less stringent (a.o. higher distance for walkers)
* Extra information added to the documentation of `load_plotinfo()`.
* function `calc_reg_plot()`: add `date_regeneration`: date of survey.

# forrescalc 0.0.1

* Initial version
