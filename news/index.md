# Changelog

## cohortBuilder 1.0.0

CRAN release: 2026-07-03

### Breaking changes

- **Filter system rewritten to S7.** Filters are now S7 objects
  (e.g. `CbFilterDiscrete`, `CbFilterRange`) with dual dispatch on
  (filter_class, source_class). Properties are accessed via `@` instead
  of closure environments. New S7 generics:
  [`cb_filter_data()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_filter_data.md),
  [`cb_get_filter_stats()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_get_filter_stats.md),
  [`cb_plot_filter_data()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_plot_filter_data.md),
  [`cb_get_filter_data()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_get_filter_data.md),
  [`cb_get_filter_defaults()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_get_filter_defaults.md),
  [`cb_filter_to_expr()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_filter_to_expr.md).
- Removed `def_filter()`, `new_filter()`, and `.as_constructor()` —
  replaced by S7 constructors.
- Switched from magrittr `%>%` to native pipe `|>`. Requires R \>=
  4.1.0.
- Renamed the cohort statistics store and its API for clarity: the
  `Cohort$new()` `cache` argument is now `compute_stats`, the
  `propagate_domains` mode `"cache"` is now `"stats"`, and the
  `get_cache()`/`update_cache()` methods are now
  `get_stats()`/`update_stats()`. The previous live-computation method
  `Cohort$get_stats()` (used by
  [`stat()`](https://r-world-devs.github.io/cohortBuilder/reference/stat.md))
  is now `calc_stats()`.

### New features

- Custom filter types can now be registered via
  [`register_filter_type()`](https://r-world-devs.github.io/cohortBuilder/reference/register_filter_type.md),
  enabling extensions without modifying core package code.
- New
  [`describe()`](https://r-world-devs.github.io/cohortBuilder/reference/describe.md)
  helper to attach metadata (descriptions) to datasets and filters.
  [`describe()`](https://r-world-devs.github.io/cohortBuilder/reference/describe.md)
  also accepts a `label` argument; when set on a variable,
  [`autofilter()`](https://r-world-devs.github.io/cohortBuilder/reference/autofilter.md)
  reuses it as the generated filter’s `name`.
- [`shape()`](https://r-world-devs.github.io/cohortBuilder/reference/shape.md)
  filter entries now include a `name` field, and the `description` field
  combines the filter- and variable-level descriptions.
- New
  [`shape()`](https://r-world-devs.github.io/cohortBuilder/reference/shape.md)
  generic extracts structured filter/dataset metadata from a source,
  including statistics (min/max for range, choices for discrete).
- New
  [`autofilter()`](https://r-world-devs.github.io/cohortBuilder/reference/autofilter.md)
  generic auto-generates filters based on column types (character/factor
  → discrete, numeric → range, Date → date_range, POSIXct →
  datetime_range). Supports `attach_as = "step"` (add as filtering step)
  or `attach_as = "meta"` (store as available filters).
- New `.class` parameter in
  [`tblist()`](https://r-world-devs.github.io/cohortBuilder/reference/tblist.md)
  to prepend custom S3 classes for method dispatch customization.
- Steps now track `pending` status — only pending steps trigger
  statistics recalculation, improving performance for multi-step
  workflows.
- [`update_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/update_filter.md)
  now supports pre/post hooks via `hook_args`.
- New `Cohort$new()` `propagate_domains` argument controls how filter
  domains are narrowed between steps: `"none"` (default), `"filter"`
  (from previous step filter values), `"stats"` (from stored statistics;
  requires `compute_stats = TRUE`), or `"data"` (scan filtered data; the
  stats-free equivalent). Backed by the
  [`.propagate_domains()`](https://r-world-devs.github.io/cohortBuilder/reference/dot-propagate_domains.md)
  source method and the S7 generics
  [`cb_intersect_domain()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_intersect_domain.md),
  [`cb_intersect_domain_values()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_intersect_domain_values.md),
  [`cb_domain_from_stats()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_domain_from_stats.md),
  and
  [`cb_domain_from_data()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_domain_from_data.md).
  Filter values are now intersected against their domain (with trimming
  messages) before filtering.
- [`set_source()`](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md)
  gains a `compute_meta_stats` argument (default from the
  `cb.source_filters_meta_stats` option) controlling whether metadata
  statistics for `available_filters` are pre-computed; when `FALSE`,
  filter domains fall back to live computation. Sources also accept an
  `available_filters` definition directly.
- Filter ids are now deterministic by default (derived from dataset and
  variable names), enabling cross-step filter matching for domain
  propagation. Override with explicit `id =`.

### AI/LLM integration

- New
  [`cb_tool()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
  system for defining LLM-compatible tool specifications (requires
  `ellmer`).
- Built-in tools:
  [`cb_tool_filters_meta()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_filters_meta.md),
  [`cb_tool_add_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_add_filters.md),
  [`cb_tool_set_filter_values()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_set_filter_values.md),
  [`cb_tool_apply_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_apply_filters.md)
  (combined add + set values).
- [`cb_register_tool()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_register_tool.md)
  and
  [`cb_register_tools()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_register_tool.md)
  register tools with an `ellmer` chat object.
- AI tool invocations can be traced by setting
  `options(cb_tool_verbose = TRUE)`, which logs the invoked tool and its
  arguments via [`message()`](https://rdrr.io/r/base/message.html).
  Logging is off by default (replaces earlier unconditional
  [`print()`](https://rdrr.io/r/base/print.html) debug output).

### Improvements

- Use `collapse` for binding operations (joins), with `verbose` option
  for diagnostics.
- Reorganized `breaks` argument for date_range filter plots.
- Extensive test coverage improvements including vdiffr snapshot tests
  for all filter plot types.
- [`cohort()`](https://r-world-devs.github.io/cohortBuilder/reference/create-cohort.md)
  now exposes the `compute_stats` and `propagate_domains` arguments, and
  [`add_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/add_filter.md)/[`rm_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/rm_filter.md)
  now expose and forward the `hook` argument (previously these were
  silently ignored).
- Comprehensive roxygen documentation added across exported and internal
  functions, plus a new `source-intelligence` vignette covering
  [`describe()`](https://r-world-devs.github.io/cohortBuilder/reference/describe.md),
  [`autofilter()`](https://r-world-devs.github.io/cohortBuilder/reference/autofilter.md),
  [`shape()`](https://r-world-devs.github.io/cohortBuilder/reference/shape.md),
  and the AI/LLM tools.

## cohortBuilder 0.4.0

CRAN release: 2026-02-24

- Multi discrete filter does not operate on
  [`dplyr::across`](https://dplyr.tidyverse.org/reference/across.html)
  and
  [`dplyr::cur_column`](https://dplyr.tidyverse.org/reference/context.html)
  anymore.
- Now cohort calculates only active filters cache while initializing
  source (results with significant performance improvement). The
  `get_cache` method computes cache when called (and the related cache
  was missing).
- Add new `datatime_filter` that handle POSIXct type.
- Move unique/distinct to
  [`collapse::funique`](https://fastverse.org/collapse/reference/funique.html).
- Replace (internally) `%in%` with custom operator using
  [`collapse::fmatch`](https://fastverse.org/collapse/reference/fmatch.html),
  that seems to be more efficient.

## cohortBuilder 0.3.0

CRAN release: 2024-09-25

- Add new filter of type `"query"` that allows to configure complex
  filtering rules with `queryBuilder` package.
- Add filter-focused `.print_filter` method responsible for printing
  filter values when calling `sum_up` on cohort.

## cohortBuilder 0.2.0

CRAN release: 2023-02-28

- Changed the way reproducible code is returned. Now more flexibility is
  allowed with using e.g. `.repro_code_tweak` method.
- The `tblist` source reproducible code is now using pipe chains for
  each dataset filtering.
- Optimized filtering with having cache computed only for active
  filters.
- Properly readjust steps and filters ids after step is removed.
- Add `.post_binding` method, that allows to modify data object when
  binding is completed.
- Fix reproducible code generation when no filters applied.

## cohortBuilder 0.1

CRAN release: 2022-06-01

- First release.
