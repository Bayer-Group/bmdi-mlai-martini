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
