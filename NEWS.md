# martini 0.5.0

* fixed package data sets. Update of {recipes} package introduced NAs in prepared ML data, which were now removed.
* some refactoring
* speed improvements in prepare_ml()
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
