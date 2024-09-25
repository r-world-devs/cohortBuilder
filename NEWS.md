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
