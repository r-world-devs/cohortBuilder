# S7 Filter Rewrite Plan

## Goal
Replace the dual S3 dispatch filter system (`filter.{type}` + `cb_filter.{type}.{source_class}`) with S7 classes and multi-dispatch generics. This is a breaking change — external packages must update.

## Current Architecture (Problems)

1. **Dual S3 dispatch hack**: `filter("discrete", ...)` → `filter.discrete()` → returns closure → closure calls `cb_filter.discrete(source)` → `cb_filter.discrete.tblist(source, ...)`. This simulates multi-dispatch using two layers of S3.
2. **Environment-based state**: Filter parameters live in closure environments. `update_filter` directly mutates `environment(filter$filter_data)`. `get_filter_state` uses `as.list(environment(filter$filter_data))`. Fragile and hard to reason about.
3. **Lots of boilerplate**: Each filter type needs: `filter.{type}()`, `cb_filter.{type}()`, and `cb_filter.{type}.{source}()` — three functions per type × source combination.
4. **Repro code generation**: Parses `body()` of closures, extracts environment vars via `as.list(environment(func))`, pattern-matches special comments like `# keep_na !value_na start`. Very brittle.

## New Architecture (S7)

### 1. S7 Filter Classes (`R/filter.R`)

Define a base `CbFilter` S7 class and subclasses for each filter type:

```r
CbFilter <- new_class("CbFilter", package = "cohortBuilder",
  properties = list(
    type = class_character,
    id = class_character,
    name = class_character,
    input_param = class_character,
    dataset = class_character,
    active = class_logical,
    description = class_any,
    step_id = new_property(class_any, default = NULL)
  )
)

CbFilterDiscrete <- new_class("CbFilterDiscrete", parent = CbFilter,
  package = "cohortBuilder",
  properties = list(
    variable = class_character,
    value = class_any,       # NA or character vector
    keep_na = class_logical
  ),
  constructor = function(id = .gen_id(), name = id, variable, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         active = getOption("cb_active_filter", default = TRUE)) {
    new_object(S7_object(),
      type = "discrete", id = id, name = name, input_param = "value",
      variable = variable, value = value, dataset = dataset,
      keep_na = keep_na, active = active, description = description
    )
  }
)
```

Similar subclasses: `CbFilterDiscreteText`, `CbFilterRange`, `CbFilterDateRange`, `CbFilterDatetimeRange`, `CbFilterMultiDiscrete`, `CbFilterQuery`.

### 2. S7 Multi-Dispatch Generics (`R/filter.R`)

Replace the closure-based methods with S7 generics that dispatch on (filter_class, source_class):

```r
# Register S3 class for tblist so S7 can dispatch on it
tblist_class <- new_S3_class("tblist")

filter_data <- new_generic("filter_data", c("filter", "source"),
  function(filter, source, data_object, ...) S7_dispatch()
)

get_filter_stats <- new_generic("get_filter_stats", c("filter", "source"),
  function(filter, source, data_object, ...) S7_dispatch()
)

plot_filter_data <- new_generic("plot_filter_data", c("filter", "source"),
  function(filter, source, data_object, ...) S7_dispatch()
)

get_filter_defaults <- new_generic("get_filter_defaults", c("filter", "source"),
  function(filter, source, data_object, cache_object, ...) S7_dispatch()
)

get_filter_data <- new_generic("get_filter_data", c("filter", "source"),
  function(filter, source, data_object, ...) S7_dispatch()
)
```

### 3. tblist Method Implementations (`R/source_tblist.R`)

Register methods for each (filter_type, tblist) combination:

```r
method(filter_data, list(CbFilterDiscrete, tblist_class)) <- function(filter, source, data_object, ...) {
  if (filter@keep_na && !identical(filter@value, NA)) {
    data_object[[filter@dataset]] <- data_object[[filter@dataset]] |>
      dplyr::filter(!!sym(filter@variable) %in% c(filter@value, NA))
  }
  # ... other cases
  attr(data_object[[filter@dataset]], "filtered") <- TRUE
  data_object
}
```

### 4. Simplified `filter()` Constructor (`R/filter.R`)

The `filter()` function becomes a simple factory — no more S3 dispatch on character type:

```r
filter <- function(type, ...) {
  constructor <- switch(type,
    discrete = CbFilterDiscrete,
    discrete_text = CbFilterDiscreteText,
    range = CbFilterRange,
    date_range = CbFilterDateRange,
    datetime_range = CbFilterDatetimeRange,
    multi_discrete = CbFilterMultiDiscrete,
    query = CbFilterQuery,
    stop(paste("Unknown filter type:", type))
  )
  constructor(...)
}
```

No more `cb_filter_constructor` class and `.as_constructor()`. The `filter()` function returns the S7 filter object directly, not a closure.

### 5. Update `eval_filter()` (`R/filter.R`)

Currently `eval_filter` calls the constructor closure with `source`. With S7, the filter object is already fully constructed — `eval_filter` just attaches the step_id:

```r
eval_filter <- function(filter_obj, step_id, source) {
  filter_obj@step_id <- step_id
  filter_obj
}
```

### 6. Update `get_filter_state()` (`R/filter.R`)

No more environment introspection. Use S7 properties:

```r
get_filter_state <- function(filter, extra_fields) {
  props <- props(filter)
  # Remove internal-only properties
  props$step_id <- NULL
  if (!is.null(extra_fields)) {
    for (field in extra_fields) {
      props[[field]] <- filter[[field]]  # or prop(filter, field)
    }
  }
  props
}
```

### 7. Update `update_filter()` in Cohort (`R/cohort_methods.R`)

Replace environment mutation with S7 property setting:

```r
# Before (S3):
# filter_env <- environment(private$steps[[step_id]]$filters[[filter_id]]$filter_data)
# filter_env[[param_name]] <- new_val

# After (S7):
filter_obj <- private$steps[[step_id]]$filters[[filter_id]]
for (param_name in params_to_change) {
  new_val <- new_args[[param_name]]
  if (!identical(prop(filter_obj, param_name), new_val)) {
    any_changed <- TRUE
    prop(filter_obj, param_name) <- new_val
  }
}
private$steps[[step_id]]$filters[[filter_id]] <- filter_obj
```

### 8. Update `run_step()` in Cohort (`R/cohort_methods.R`)

Change how filters are applied — pass source to the generic:

```r
# Before:
# temp_data_object <- temp_data_object %>% data_filter$filter_data()

# After:
for (filter_id in active_filters) {
  f <- self$get_filter(step_id, filter_id)
  temp_data_object <- filter_data(f, private$source, temp_data_object)
}
```

Similarly update `get_stats`, `plot_data`, `get_data`, `get_defaults` calls throughout Cohort.

### 9. Rewrite `repro_code_utils.R`

Replace environment-based code generation with S7 property-based approach:

- `parse_filter_expr()`: Instead of parsing closure body + extracting env vars, build the expression from filter properties directly. Each filter type gets a method for a new generic `filter_to_expr()`.
- `get_filter_state()` already returns clean property list — use that.
- Remove the `# keep_na !value_na start/end` comment pattern from filter implementations.

```r
filter_to_expr <- new_generic("filter_to_expr", c("filter", "source"),
  function(filter, source, ...) S7_dispatch()
)

method(filter_to_expr, list(CbFilterDiscrete, tblist_class)) <- function(filter, source, ...) {
  # Build expression directly from properties
  if (filter@keep_na && !identical(filter@value, NA)) {
    rlang::expr(
      data_object[[!!filter@dataset]] <- data_object[[!!filter@dataset]] |>
        dplyr::filter(!!sym(filter@variable) %in% !!c(filter@value, NA))
    )
  }
  # ... other cases
}
```

### 10. Update `def_filter()` and `new_filter()`

- `def_filter()`: Deprecate or remove. It's replaced by S7 class constructors.
- `new_filter()`: Update the template to generate S7 class + method registration instead of S3 functions.

### 11. Update `.print_filter`

Convert to S7 generic dispatching on filter class:

```r
method(print, CbFilter) <- function(x, ...) {
  # default print using properties
}
method(print, CbFilterQuery) <- function(x, ...) {
  # query-specific print
}
```

### 12. Update `%->%` operator

Check for S7 filter class instead of `cb_filter_constructor`:

```r
`%->%` <- function(x, object) {
  if (inherits(object, "cb_step")) add_step(x, object)
  else if (S7_inherits(object, CbFilter)) add_filter(x, object)
  else `%>%`(x, object)
}
```

### 13. Package Infrastructure

- Add `S7` to `Imports` in `DESCRIPTION`
- Add `S7::methods_register()` in `.onLoad()`
- Update `NAMESPACE`: remove old S3method registrations for `filter.*` and `cb_filter.*.*`, add S7 exports
- Add `@rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")` for backward compat

---

## Files to Modify

| File | Changes |
|------|---------|
| `DESCRIPTION` | Add S7 to Imports |
| `R/cohortBuilder-package.R` | Add `.onLoad` with `S7::methods_register()` |
| `R/filter.R` | Major rewrite: S7 classes, generics, simplified `filter()` factory |
| `R/source_tblist.R` | Rewrite `cb_filter.*.tblist` as S7 method registrations |
| `R/cohort_methods.R` | Update `update_filter`, `run_step`, `get_stats`, `plot_data`, etc. |
| `R/source_methods.R` | Update `update_filter` in Source class |
| `R/repro_code_utils.R` | Rewrite code generation using S7 properties |
| `R/step.R` | Minor: update `eval_step_filters` |
| `R/attrition.R` | Minor: update filter state access if needed |
| `tests/testthat/` | Update all test files to use new API |
| `vignettes/custom-filters.Rmd` | Rewrite filter creation guide |
| `inst/filter_template` | Update template |

## Migration Summary for External Packages

External packages (e.g., shinyCohortBuilder, database source packages) need to:

1. **Filter type packages**: Define S7 subclass of `CbFilter` + register `filter_data`/`get_filter_stats`/etc. methods
2. **Source packages**: Register S7 methods for `(ExistingFilterType, NewSourceClass)` combinations
3. **shinyCohortBuilder**: Update all `environment(filter$filter_data)` access to use `prop(filter, name)` / `filter@name`

## Implementation Order

1. Add S7 dependency and package infrastructure
2. Define S7 filter classes in `R/filter.R`
3. Define S7 generics (filter_data, get_filter_stats, etc.)
4. Implement tblist methods in `R/source_tblist.R`
5. Update `filter()` factory and `eval_filter()`
6. Update Cohort methods (`run_step`, `update_filter`, `get_filter_state`, etc.)
7. Update Source methods
8. Rewrite `repro_code_utils.R`
9. Update attrition, printing, and `%->%` operator
10. Update tests
11. Update vignettes and documentation
