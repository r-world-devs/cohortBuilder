# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

cohortBuilder is a data source-agnostic filtering framework for cohort creation in R. It provides a common API for filtering data stored in different data models (data frames, databases, etc.) with reproducible R code generation. The built-in source type is `tblist` (named list of tibbles); other sources (e.g., database-backed) are implemented in separate packages.

It pairs with `shinyCohortBuilder` for interactive Shiny GUIs.

## Development Commands

```r
devtools::load_all()             # Load package for interactive use
devtools::test()                 # Run full test suite
devtools::test(filter = "filter") # Run tests matching pattern (matches test-filter.R)
devtools::document()             # Regenerate man/ and NAMESPACE from roxygen2
devtools::check()                # Full R CMD check (uses --no-manual --compact-vignettes)
lintr::lint_package()            # Lint (config in .lintr)
```

## Architecture

### Two R6 Classes

**Source** (`R/source_methods.R`) — Wraps a data connection (`dtconn`) with metadata: primary keys, binding keys, available filters, and computed stats. Defines S3 generics for source-type-specific operations (`.init_step()`, `.pre_filtering()`, `.post_filtering()`, `.collect_data()`, `.get_stats()`, `.run_binding()`, etc.).

**Cohort** (`R/cohort_methods.R`) — Orchestrates the filtering workflow. Manages steps (ordered filter groups), executes the data pipeline via `run_flow()`, and provides state serialization (`get_state()`/`restore()`), reproducible code generation (`get_code()`), and statistics/attrition reporting. Supports pre/post hooks for most operations.

### Filter System (S7 Dual Dispatch)

Filters use S7 classes with dual dispatch on (filter_class, source_class):

1. `filter(type, ...)` constructs an S7 object (e.g., `CbFilterDiscrete`, `CbFilterRange`)
2. S7 generics dispatch on both filter type and source type:
   - `cb_filter_data(filter, source)` — apply filter to data
   - `cb_get_filter_stats(filter, source)` — compute filter statistics
   - `cb_plot_filter_data(filter, source)` — plot filter data
   - `cb_get_filter_defaults(filter, source)` — get default parameter values
   - `cb_filter_to_expr(filter, source)` — generate reproducible code expression

Seven built-in filter types: `discrete`, `discrete_text`, `range`, `date_range`, `datetime_range`, `multi_discrete`, `query`. Custom filters can be registered via `register_filter_type()`.

### Step Execution Pipeline

`Cohort$run_step()` executes in order:
1. `.pre_filtering()` — source-specific preprocessing
2. `cb_filter_data()` — apply each active filter
3. `.post_filtering()` — source-specific postprocessing
4. `.run_binding()` / `.post_binding()` — cascade filter results across related datasets via `binding_keys`
5. `update_cache()` — compute and cache filter stats

### Extending to New Source Types

To support a new data backend, implement:
- `set_source.{type}()` S3 method
- Source-layer S3 methods (`.init_step`, `.pre_filtering`, `.collect_data`, `.get_stats`, etc.)
- S7 methods for each filter type × source type combination (`cb_filter_data`, `cb_get_filter_stats`, etc.)

The `tblist` implementation in `R/source_tblist.R` serves as the reference (60+ S7 method implementations).

### Key Supporting Modules

- `R/step.R` — Step creation and management
- `R/bind_keys.R` — Dataset relationship definitions (`primary_keys`, `bind_key`, `data_key`)
- `R/hooks.R` — Hook registration and execution system
- `R/repro_code_utils.R` — Expression manipulation for reproducible code generation
- `R/attrition.R` — Attrition statistics and ggplot2 visualization

## Linting Rules

Key non-default rules (see `.lintr`):
- Max line length: 120
- Pipe style: `|>` (native pipe), not `%>%`
- Banned: `Sys.setenv()`, `mapply()`, `unique()` (use `collapse::funique`)
- No object name or object usage linting
- Hanging indent style: tidy

## Testing

- testthat edition 3
- Snapshot tests via vdiffr for plot validation (`tests/testthat/_snaps/`)
- Test data: `tests/data/sakila/sakila.rda` for binding key scenarios
- Tests are paired with source files: `test-filter.R` ↔ `R/filter.R`, etc.

## Custom Operators

- `%->%` — Pipe that dispatches `add_filter()` or `add_step()` when RHS is a filter/step, otherwise behaves as standard pipe
- `%:::%` — Convenience for `getFromNamespace()`
- `%in%` — Overridden to use `collapse::fmatch` for performance
