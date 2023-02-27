# cohortBuilder (development version)

* Changed the way reproducible code is returned. Now more flexibility is allowed with using e.g. `.repro_code_tweak` method.
* The `tblist` source reproducible code is now using pipe chains for each dataset filtering.
* Optimized filtering with having cache computed only for active filters.
* Properly readjust steps and filters ids after step is removed.

# cohortBuilder 0.1.1

* Add `.post_binding` method, that allows to modify data object when binding is completed.

# cohortBuilder 0.1

* First release.
