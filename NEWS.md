# forrescalc 0.0.2

* functions `read_forresdat()`, `read_forresdat_table()` and
  `from_forresdat_to_access()`: extra arguments `git_ref_type` and
  `git_reference` that allow to read branches and commits from `forresdat`
  (not only releases)
* minor bug fixes and documentation corrections
* check functions made a bit less stringent (a.o. higher distance for walkers)
* minor corrections of check functions (a.o. if no survey, no need to test 
  missing values)

# forrescalc 0.0.1

* Initial version

# forrescalc 0.0.2

* Corrections: 

- Fixed incorrect zero values for records where game impact was not recorded. 
(functions `calc_reg_plot_height(_species)` en 
`calc_reg_core_area_height_species`)


* Enhancements:

- Extra information added to the documentation of `load_plotinfo()`.

- Removed variable `vol_deadw_m3_ha`, which previously combined standing and 
lying deadwood volumes, to avoid ambiguity about the included components.
(functions `calc_dendro_plot_species()`)

- Extended `load_plotinfo()` to include thresholds and LIS survey status.

- `dbh_min_a3` replaced with `dbh_min_a3_alive` in the functions
`load_data_dendrometry()` and `load_plotinfo()`, to be more in line with 
`dbh_min_a3_dead` and avoid confusion. 
Idem for `dbh_min_a4`.

- Variable `year_main_survey` - imported through a sql-query from fieldmap - 
replaced by `year_recorded` in the functions `load_data_vegetation()` 
and `load_data_regeneration()` to avoid confusion.

- Variable `year_main_survey` - calculated - replaced by `year` in the functions 
`load_data_vegetation()` and `calc_veg_plot()` to avoid confusion and be more 
in line with the function `load_data_regeneration()`.

- Date of survey `date_regeneration` added to the function `calc_reg_plot()`.

- Test-functions `test1_load_functions_basics.R`, 
`test2_calculate_dendrometry_basics.R`, `test3_calculate_regeneration_basics.R`, 
`test4_calculate_vegetation_basics.R` and `test6_create_statistics.R` adapted 
to address the changes mentioned above.



