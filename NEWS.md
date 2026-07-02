# cohortBuilder 1.0.0

## Breaking changes

* **Filter system rewritten to S7.** Filters are now S7 objects (e.g. `CbFilterDiscrete`, `CbFilterRange`)
  with dual dispatch on (filter_class, source_class). Properties are accessed via `@` instead of
  closure environments. New S7 generics: `cb_filter_data()`, `cb_get_filter_stats()`,
  `cb_plot_filter_data()`, `cb_get_filter_data()`, `cb_get_filter_defaults()`, `cb_filter_to_expr()`.
* Removed `def_filter()`, `new_filter()`, and `.as_constructor()` — replaced by S7 constructors.
* Switched from magrittr `%>%` to native pipe `|>`. Requires R >= 4.1.0.
* Renamed the cohort statistics store and its API for clarity: the `Cohort$new()` `cache`
  argument is now `compute_stats`, the `propagate_domains` mode `"cache"` is now `"stats"`,
  and the `get_cache()`/`update_cache()` methods are now `get_stats()`/`update_stats()`.
  The previous live-computation method `Cohort$get_stats()` (used by `stat()`) is now `calc_stats()`.

## New features

* Custom filter types can now be registered via `register_filter_type()`, enabling extensions
  without modifying core package code.
* New `describe()` helper to attach metadata (descriptions) to datasets and filters.
  `describe()` also accepts a `label` argument; when set on a variable, `autofilter()`
  reuses it as the generated filter's `name`.
* `shape()` filter entries now include a `name` field, and the `description` field
  combines the filter- and variable-level descriptions.
* New `shape()` generic extracts structured filter/dataset metadata from a source,
  including statistics (min/max for range, choices for discrete).
* New `autofilter()` generic auto-generates filters based on column types
  (character/factor → discrete, numeric → range, Date → date_range, POSIXct → datetime_range).
  Supports `attach_as = "step"` (add as filtering step) or `attach_as = "meta"` (store as available filters).
* New `.class` parameter in `tblist()` to prepend custom S3 classes for method dispatch customization.
* Steps now track `pending` status — only pending steps trigger statistics recalculation,
  improving performance for multi-step workflows.
* `update_filter()` now supports pre/post hooks via `hook_args`.
* New `Cohort$new()` `propagate_domains` argument controls how filter domains are narrowed
  between steps: `"none"` (default), `"filter"` (from previous step filter values),
  `"stats"` (from stored statistics; requires `compute_stats = TRUE`), or `"data"`
  (scan filtered data; the stats-free equivalent). Backed by the `.propagate_domains()`
  source method and the S7 generics `cb_intersect_domain()`, `cb_intersect_domain_values()`,
  `cb_domain_from_stats()`, and `cb_domain_from_data()`. Filter values are now intersected
  against their domain (with trimming messages) before filtering.
* `set_source()` gains a `compute_meta_stats` argument (default from the
  `cb.source_filters_meta_stats` option) controlling whether metadata statistics for
  `available_filters` are pre-computed; when `FALSE`, filter domains fall back to live
  computation. Sources also accept an `available_filters` definition directly.
* Filter ids are now deterministic by default (derived from dataset and variable names),
  enabling cross-step filter matching for domain propagation. Override with explicit `id =`.

## AI/LLM integration

* New `cb_tool()` system for defining LLM-compatible tool specifications (requires `ellmer`).
* Built-in tools: `cb_tool_filters_meta()`, `cb_tool_add_filters()`, `cb_tool_set_filter_values()`,
  `cb_tool_apply_filters()` (combined add + set values).
* `cb_register_tool()` and `cb_register_tools()` register tools with an `ellmer` chat object.
* AI tool invocations can be traced by setting `options(cb_tool_verbose = TRUE)`, which logs the
  invoked tool and its arguments via `message()`. Logging is off by default (replaces earlier
  unconditional `print()` debug output).

## Improvements

* Use `collapse` for binding operations (joins), with `verbose` option for diagnostics.
* Reorganized `breaks` argument for date_range filter plots.
* Extensive test coverage improvements including vdiffr snapshot tests for all filter plot types.
* `cohort()` now exposes the `compute_stats` and `propagate_domains` arguments, and
  `add_filter()`/`rm_filter()` now expose and forward the `hook` argument (previously these
  were silently ignored).
* Comprehensive roxygen documentation added across exported and internal functions, plus a new
  `source-intelligence` vignette covering `describe()`, `autofilter()`, `shape()`, and the
  AI/LLM tools.

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
