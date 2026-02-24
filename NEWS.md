# cohortBuilder 0.4.0

* Multi discrete filter does not operate on `dplyr::across` and `dplyr::cur_column` anymore.
* Now cohort calculates only active filters cache while initializing source (results with significant performance improvement). 
  The `get_cache` method computes cache when called (and the related cache was missing).
* Add new `datatime_filter` that handle POSIXct type.
* Move unique/distinct to `collapse::funique`.
* Replace (internally) `%in%` with custom operator using `collapse::fmatch`, that seems to be more efficient.

# cohortBuilder 0.3.0

* Add new filter of type `"query"` that allows to configure complex filtering rules with `queryBuilder` package.
* Add filter-focused `.print_filter` method responsible for printing filter values when calling `sum_up` on cohort.

# cohortBuilder 0.2.0

* Changed the way reproducible code is returned. Now more flexibility is allowed with using e.g. `.repro_code_tweak` method.
* The `tblist` source reproducible code is now using pipe chains for each dataset filtering.
* Optimized filtering with having cache computed only for active filters.
* Properly readjust steps and filters ids after step is removed.
* Add `.post_binding` method, that allows to modify data object when binding is completed.
* Fix reproducible code generation when no filters applied.

# cohortBuilder 0.1

* First release.
