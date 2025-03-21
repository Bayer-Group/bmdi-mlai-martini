# martini 0.6.4

major changes

* to accommodate for handling of adsl data sets in ADaM 2.0 format, 
the structure of `fct_levels` entry was changed to name value pairs

* added function `adjust_spec_filter()`, removing `append` argument from
`adjust_spec()`

* `adjust_spec()` now provides extensive checks of user defined modifications to 
a `spec` object. Part of them relies on the data being available, which is why
we strongly recommend to attach data in `adam_spec()`, if possible, which is 
why the default for `attach_data` in `adam_spec()` is now `TRUE`

# martini 0.6.3

* preparations for bay-open: added code of conduct, updated license and added code owner and contributing information

# martini 0.6.2

* removed ´corrr´ package dependency
* introducing github actions

# martini 0.6.1

* updated output of correlation handling (`removals$cols` structure and addition of alternative labels in dictionary)

# martini 0.6.0

* in rare cases, samples could end up being removed from training data set 
due to incomplete imputation from `recipes::step_impute_knn()`. 
For this edge case, additional imputation steps were added to ensure full data set 
usage for training (`recipes::step_impute_median()` for numerics and `recipes::step_impute_mode()` for factors.)

* update example objects, since `prepare_ml()` output object  
  * has new entry high_corr for more transparency on feature dropping for correlation (`recipes::step_corr()`) 
  * no longer contains redundant slot for split object

* variables that are defined in `prepare_ml()`'s `vars_keep_corr` but not present in data set are ignored (previously an error was thrown)
* improved handling of independent data sets for prediction
* improved test suite

# martini 0.5.1

* Added experimental parameter `rm` in `build()` to allow for the preparation of a 
wide data set suitable for repeated measurement outcomes 
(one row is a subject at a specific time point)

# martini 0.5.0

* fixed package data sets. Update of {recipes} package introduced NAs in prepared ML data, which were now removed.
* some refactoring
* speed improvements in prepare_ml() (affects imputation seed)
* added some tests

# martini 0.4.3

* update of example data sets
* minor bug fixes related to incomplete bds data sets (e.g. missing units)

# martini 0.4.2

* improved messaging (wip)
* updated docu
* switched to testthat 3e and updated tests
* some refactoring

# martini 0.4.1

## Breaking changes
* change one_hot default to FALSE. 

# martini 0.4.0

* `build_bds()´ converts values character values (e.g. AVALC) to either numerics or factors, based on observed values per parameter (e.g. PARAMCD)  

# martini 0.3.4

* Added a `NEWS.md` file to track changes to the package.
* Dictionary (if available) is updated if variables are added/dropped with `adjust_adsl()`.
* If data is attached to the spec object, usage of `adjust_adsl()` and `adjust_spec()` will 
update the data_info and filter check attributes shown by the print method  
