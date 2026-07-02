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

**Source** (`R/source_methods.R`) — Wraps a data connection (`dtconn`) with metadata: primary keys, binding keys, available filters, and computed stats. Defines S3 generics for source-type-specific operations (`.init_step()`, `.pre_filtering()`, `.post_filtering()`, `.collect_data()`, `.get_stats()`, `.run_binding()`, etc.). The `compute_meta_stats` argument (default from option `cb.source_filters_meta_stats`, `TRUE`) controls whether metadata statistics for `available_filters` are pre-computed into `meta_stats`; when `FALSE`, filter domains fall back to live computation.

**Cohort** (`R/cohort_methods.R`) — Orchestrates the filtering workflow. Manages steps (ordered filter groups), executes the data pipeline via `run_flow()`, and provides state serialization (`get_state()`/`restore()`), reproducible code generation (`get_code()`), and statistics/attrition reporting. Supports pre/post hooks for most operations (including `update_filter`).

Key `Cohort$new()` arguments:
- `compute_stats` (default `TRUE`) — compute and store filter/step statistics after each step. Set `FALSE` for metadata-only operation.
- `propagate_domains` — domain propagation mode between steps: `"none"` (default), `"filter"` (from previous step filter values), `"stats"` (from stored statistics; requires `compute_stats = TRUE`), or `"data"` (scan filtered data; the stats-free equivalent).

The per-step statistics store (`private$stats`) is keyed by step id; slot `N` is both step N's post-filtering stats and step N+1's pre-filtering stats, and nests data stats under `$source` and filter stats under `$filters`. Methods: `get_stats()`/`update_stats()` read/write the store; `calc_stats()` computes stats live (used by `stat()`).

### Filter System (S7 Dual Dispatch)

Filters use S7 classes with dual dispatch on (filter_class, source_class):

1. `filter(type, ...)` constructs an S7 object (e.g., `CbFilterDiscrete`, `CbFilterRange`)
2. S7 generics dispatch on both filter type and source type:
   - `cb_filter_data(filter, source)` — apply filter to data
   - `cb_get_filter_stats(filter, source)` — compute filter statistics
   - `cb_plot_filter_data(filter, source)` — plot filter data
   - `cb_get_filter_defaults(filter, source)` — get default parameter values
   - `cb_filter_to_expr(filter, source)` — generate reproducible code expression
3. S7 generics dispatching on filter type only (extensible for custom filters):
   - `cb_intersect_domain(filter)` — intersect filter value with its domain
   - `cb_domain_from_stats(filter, stats)` — extract domain from stored statistics
4. S7 dual-dispatch generic for domain extraction from data:
   - `cb_domain_from_data(filter, source, data_object)` — extract domain from data

Seven built-in filter types: `discrete`, `discrete_text`, `range`, `date_range`, `datetime_range`, `multi_discrete`, `query`. Custom filters can be registered via `register_filter_type()`.

### Filter IDs

Filter IDs are deterministic by default, generated from filter properties via `.default_filter_id()`:
- Single-variable filters: `<dataset>-<variable>` (e.g., `iris-Species`)
- `multi_discrete`: `<dataset>-<var1>-<var2>-md`
- `query`: `<dataset>-<var1>-<var2>-q`

Non-alphanumeric characters are stripped. For >3 variables, the ID is truncated to 3 variables + a 4-char hash suffix. Users can override with explicit `id =`. Cross-step filter matching (domain propagation) uses ID equality.

### Step Execution Pipeline

`Cohort$run_step()` executes in order:
1. `.pre_filtering()` — source-specific preprocessing
2. `cb_filter_data()` — apply each active filter
3. `.post_filtering()` — source-specific postprocessing
4. `.run_binding()` / `.post_binding()` — cascade filter results across related datasets via `binding_keys`
5. `update_stats()` — compute and store filter stats

### Extending to New Source Types

To support a new data backend, implement:
- `set_source.{type}()` S3 method
- Source-layer S3 methods (`.init_step`, `.pre_filtering`, `.collect_data`, `.get_stats`, `.propagate_domains`, etc.)
- S7 methods for each filter type × source type combination (`cb_filter_data`, `cb_get_filter_stats`, `cb_domain_from_data`, etc.)

To define a custom filter type, implement S7 methods for:
- `cb_intersect_domain(filter)` — how value intersects with domain (default: returns raw value)
- `cb_domain_from_stats(filter, stats)` — domain extraction from stored stats (default: `NULL`)
- `cb_domain_from_data(filter, source, data_object)` — domain extraction from data (default: `NULL`)

The `tblist` implementation in `R/source_tblist.R` serves as the reference (60+ S7 method implementations). `tblist(..., .class = NULL)` accepts an optional `.class` parameter for adding custom subclasses, enabling S3 method overrides for specialized source types.

### Source Intelligence (`shape`, `autofilter`, `describe`)

- `describe(text, ...)` — Creates a description object (text + extra fields) for datasets/filters
- `autofilter(source, attach_as)` — S3 generic; auto-generates filters from data types using filter rules (`rule_character`, `rule_factor`, `rule_numeric`, etc.). `attach_as = "step"` adds filters as a step; `attach_as = "meta"` stores them in `source$available_filters`
- `shape(source)` — S3 generic; returns a structured list `list(datasets, filters)` for programmatic/LLM inspection. `datasets` maps dataset name → description text; `filters` is keyed by filter id, each entry being `list(dataset, type, description, variables, domain)` (domain falls back to `cb_domain_from_stats` on `meta_stats` when `filter@domain` is `NULL`). Called with a `field`/`subfield` (`shape(source, field, subfield)`) instead returns description text lookup (used by `Cohort$show_help()`)
- `description(cohort, ...)` — Retrieves descriptions; supports custom modifier via `cb_help_modifier` option

### Pending Step System

Cohort methods `set_pending(step_id)` and `is_pending(step_id)` track whether a step needs recalculation. Steps are marked pending when filters are added/updated/removed, and resolved after `run_step()`. This optimizes performance by skipping statistics recomputation for unchanged steps.

### AI/LLM Tool Integration (`R/ai_tools.R`)

`cb_tool` S3 class wraps tool definitions (function, name, description, arguments) for registration with `ellmer` chat objects. Four built-in tool factories:

- `cb_tool_filters_meta(cohort)` — returns filter metadata JSON via `shape()`
- `cb_tool_add_filters(cohort)` — adds filters without setting values
- `cb_tool_set_filter_values(cohort)` — updates values on existing filters
- `cb_tool_apply_filters(cohort)` — adds filters and sets values in one call

Registration helpers: `cb_register_tool(chat, tool)` and `cb_register_tools(chat, cohort)` (registers all four). Requires `ellmer` (suggested dependency).

### Key Supporting Modules

- `R/step.R` — Step creation and management
- `R/bind_keys.R` — Dataset relationship definitions (`primary_keys`, `bind_key`, `data_key`)
- `R/hooks.R` — Hook registration and execution system
- `R/repro_code_utils.R` — Expression manipulation for reproducible code generation
- `R/attrition.R` — Attrition statistics and ggplot2 visualization
- `R/ai_tools.R` — LLM tool definitions and ellmer registration

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
