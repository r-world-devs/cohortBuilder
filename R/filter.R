static_params <- c("type", "id", "name")

# S3 class registration for S7 dispatch
tblist_class <- S7::new_S3_class("tblist")

# -- Filter Type Registry -----------------------------------------------------

#' Environment storing registered filter type constructors.
#' @keywords internal
.filter_registry <- new.env(parent = emptyenv())

#' Register a custom filter type
#'
#' Registers an S7 filter constructor so it can be used with \code{\link{filter}("type", ...)}.
#' The constructor must return an object inheriting from \link{CbFilter}.
#'
#' @param type Character string identifying the filter type.
#' @param constructor S7 class constructor (e.g. created with [S7::new_class()]).
#'
#' @examples
#' \dontrun{
#' MyCbFilter <- S7::new_class("MyCbFilter",
#'   parent = CbFilter,
#'   package = "mypackage",
#'   properties = list(dataset = S7::class_character, variable = S7::class_character),
#'   constructor = function(id = NULL, name = NULL, variable, dataset,
#'                          description = NULL, domain = NULL, ...) {
#'     id <- id %||% .default_filter_id(dataset, variable)
#'     name <- name %||% id
#'     S7::new_object(S7::S7_object(),
#'       type = "my_filter", id = id, name = name,
#'       dataset = dataset, variable = variable,
#'       active = TRUE, description = description, domain = domain,
#'       extra = list(...), private = list(input_param = "value")
#'     )
#'   }
#' )
#' register_filter_type("my_filter", MyCbFilter)
#' # Now filter("my_filter", ...) works
#' }
#'
#' @export
register_filter_type <- function(type, constructor) {
  if (!is.character(type) || length(type) != 1L) {
    stop("`type` must be a single character string.")
  }
  .filter_registry[[type]] <- constructor
  invisible(type)
}

# -- S7 Filter Classes --------------------------------------------------------

#' Base class for all cohortBuilder filters
#'
#' @param type Filter type string.
#' @param id Filter identifier.
#' @param name Filter display name.
#' @param active Whether the filter is active.
#' @param description Optional filter description.
#' @param domain Optional domain constraining valid filter values. Structure depends on filter
#'   type: character vector for discrete, 2-length vector for range, named list for multi_discrete.
#'   When set, filter values are intersected with the domain. When value is unset (`NA`) and
#'   domain is provided, the domain serves as the effective value.
#' @param step_id Step identifier (set when filter is attached to a step).
#' @param extra Named list of extra parameters.
#'
#' @export
CbFilter <- S7::new_class("CbFilter",
  package = "cohortBuilder",
  properties = list(
    type = S7::class_character,
    id = S7::class_character,
    name = S7::class_character,
    active = S7::class_logical,
    description = S7::class_any,
    domain = S7::new_property(S7::class_any, default = NULL),
    step_id = S7::new_property(S7::class_any, default = NULL),
    extra = S7::new_property(S7::class_any, default = list()),
    private = S7::new_property(S7::class_any, default = list())
  )
)

#' Discrete filter class
#'
#' Filters data by matching a variable against a set of discrete values.
#'
#' @param id Filter identifier.
#' @param name Filter display name (defaults to `id`).
#' @param variable Column name to filter on.
#' @param value Values to keep. `NA` means no filtering.
#' @param dataset Dataset name.
#' @param keep_na If `TRUE`, NA values are retained.
#' @param description Optional description.
#' @param active If `FALSE`, filter is skipped.
#' @param ... Extra parameters.
#'
#' @export
CbFilterDiscrete <- S7::new_class("CbFilterDiscrete",
  parent = CbFilter,
  package = "cohortBuilder",
  properties = list(
    dataset = S7::class_character,
    variable = S7::class_character,
    value = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = NULL, name = NULL, variable, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    id <- id %||% .default_filter_id(dataset, variable)
    name <- name %||% variable
    S7::new_object(S7::S7_object(),
      type = "discrete", id = id, name = name,
      variable = variable, value = value, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      domain = domain,
      extra = list(...), private = list(input_param = "value")
    )
  }
)

#' Discrete text filter class
#'
#' Filters data by matching a variable against comma-separated text values.
#'
#' @inheritParams CbFilterDiscrete
#' @export
CbFilterDiscreteText <- S7::new_class("CbFilterDiscreteText",
  parent = CbFilter,
  package = "cohortBuilder",
  properties = list(
    dataset = S7::class_character,
    variable = S7::class_character,
    value = S7::class_any,
    keep_na = S7::new_property(S7::class_logical, default = TRUE)
  ),
  constructor = function(id = NULL, name = NULL, variable, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    id <- id %||% .default_filter_id(dataset, variable)
    name <- name %||% variable
    S7::new_object(S7::S7_object(),
      type = "discrete_text", id = id, name = name,
      variable = variable, value = value, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      domain = domain,
      extra = list(...), private = list(input_param = "value")
    )
  }
)

#' Range filter class
#'
#' Filters data by a numeric range.
#'
#' @param range Numeric vector of length 2 (min, max). `NA` means no filtering.
#' @inheritParams CbFilterDiscrete
#' @export
CbFilterRange <- S7::new_class("CbFilterRange",
  parent = CbFilter,
  package = "cohortBuilder",
  properties = list(
    dataset = S7::class_character,
    variable = S7::class_character,
    range = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = NULL, name = NULL, variable, range = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    id <- id %||% .default_filter_id(dataset, variable)
    name <- name %||% variable
    S7::new_object(S7::S7_object(),
      type = "range", id = id, name = name,
      variable = variable, range = range, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      domain = domain,
      extra = list(...), private = list(input_param = "range")
    )
  }
)

#' Date range filter class
#'
#' Filters data by a date range.
#'
#' @inheritParams CbFilterRange
#' @export
CbFilterDateRange <- S7::new_class("CbFilterDateRange",
  parent = CbFilter,
  package = "cohortBuilder",
  properties = list(
    dataset = S7::class_character,
    variable = S7::class_character,
    range = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = NULL, name = NULL, variable, range = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    id <- id %||% .default_filter_id(dataset, variable)
    name <- name %||% variable
    S7::new_object(S7::S7_object(),
      type = "date_range", id = id, name = name,
      variable = variable, range = range, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      domain = domain,
      extra = list(...), private = list(input_param = "range")
    )
  }
)

#' Datetime range filter class
#'
#' Filters data by a datetime (POSIXct) range.
#'
#' @inheritParams CbFilterRange
#' @export
CbFilterDatetimeRange <- S7::new_class("CbFilterDatetimeRange",
  parent = CbFilter,
  package = "cohortBuilder",
  properties = list(
    dataset = S7::class_character,
    variable = S7::class_character,
    range = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = NULL, name = NULL, variable, range = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    id <- id %||% .default_filter_id(dataset, variable)
    name <- name %||% variable
    S7::new_object(S7::S7_object(),
      type = "datetime_range", id = id, name = name,
      variable = variable, range = range, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      domain = domain,
      extra = list(...), private = list(input_param = "range")
    )
  }
)

#' Multi-discrete filter class
#'
#' Filters data by matching multiple variables against sets of discrete values.
#'
#' @param variables Vector of column names to filter on.
#' @param values Named list of values to filter by, keyed by variable name.
#' @inheritParams CbFilterDiscrete
#' @export
CbFilterMultiDiscrete <- S7::new_class("CbFilterMultiDiscrete",
  parent = CbFilter,
  package = "cohortBuilder",
  properties = list(
    dataset = S7::class_character,
    variables = S7::class_any,
    values = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = NULL, name = NULL, values, variables,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    id <- id %||% .default_filter_id(dataset, variables, suffix = "md")
    name <- name %||% .default_filter_name(variables, "multi_discrete")
    S7::new_object(S7::S7_object(),
      type = "multi_discrete", id = id, name = name,
      variables = variables, values = values, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      domain = domain,
      extra = list(...), private = list(input_param = "values")
    )
  }
)

#' Query filter class
#'
#' Filters data using a queryBuilder query object.
#'
#' @param variables Vector of column names used in the query.
#' @param value Query object (from queryBuilder package). `NA` means no filtering.
#' @inheritParams CbFilterDiscrete
#' @export
CbFilterQuery <- S7::new_class("CbFilterQuery",
  parent = CbFilter,
  package = "cohortBuilder",
  properties = list(
    dataset = S7::class_character,
    variables = S7::class_any,
    value = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = NULL, name = NULL, variables, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    id <- id %||% .default_filter_id(dataset, variables, suffix = "q")
    name <- name %||% .default_filter_name(variables, "query")
    S7::new_object(S7::S7_object(),
      type = "query", id = id, name = name,
      variables = variables, value = value, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      domain = domain,
      extra = list(...), private = list(input_param = "value")
    )
  }
)

# -- S7 Generics (multi-dispatch on filter + source) --------------------------

#' Apply filter to data object
#'
#' @param filter S7 filter object.
#' @param source Source object.
#' @param data_object Data object to filter.
#' @param ... Additional arguments.
#' @return Filtered data object.
#' @export
cb_filter_data <- S7::new_generic("cb_filter_data", c("filter", "source"))

#' Get filter statistics
#'
#' @param filter S7 filter object.
#' @param source Source object.
#' @param data_object Data object to compute statistics from.
#' @param ... Additional arguments.
#' @return List of statistics.
#' @export
cb_get_filter_stats <- S7::new_generic("cb_get_filter_stats", c("filter", "source"))

#' Plot filter data
#'
#' @param filter S7 filter object.
#' @param source Source object.
#' @param data_object Data object to plot.
#' @param ... Additional arguments passed to plotting functions.
#' @return Plot side effect.
#' @export
cb_plot_filter_data <- S7::new_generic("cb_plot_filter_data", c("filter", "source"))

#' Get filter-related data
#'
#' @param filter S7 filter object.
#' @param source Source object.
#' @param data_object Data object.
#' @param ... Additional arguments.
#' @return Filter-related data subset.
#' @export
cb_get_filter_data <- S7::new_generic("cb_get_filter_data", c("filter", "source"))

#' Get filter default values
#'
#' @param filter S7 filter object.
#' @param source Source object.
#' @param data_object Data object.
#' @param cache_object Cached statistics object.
#' @param ... Additional arguments.
#' @return Named list of default parameter values.
#' @export
cb_get_filter_defaults <- S7::new_generic("cb_get_filter_defaults", c("filter", "source"))

#' Generate reproducible code expression for filter
#'
#' @param filter S7 filter object.
#' @param source Source object.
#' @param ... Additional arguments.
#' @return An R expression representing the filter operation.
#' @export
cb_filter_to_expr <- S7::new_generic("cb_filter_to_expr", c("filter", "source"))

# -- Helper functions ---------------------------------------------------------

#' Generate random ID
#'
#' @return A character type value.
#' @export
.gen_id <- function() {
  paste0(
    paste(sample(LETTERS, 5L, TRUE), collapse = ""),
    round(as.numeric(Sys.time()) * 1000L)
  )
}

#' Generate a deterministic filter ID from dataset and variable names.
#'
#' @param dataset Dataset name.
#' @param variables Character vector of variable names.
#' @param suffix Optional suffix (e.g. `"md"`, `"q"`).
#' @return A single character string suitable for use as a filter ID.
#' @keywords internal
.default_filter_id <- function(dataset, variables, suffix = NULL) {
  sanitize <- function(x) gsub("[^[:alnum:]]", "", x)
  parts <- sanitize(c(dataset, head(variables, 3L)))
  if (length(variables) > 3L) {
    parts <- c(parts, substring(rlang::hash(sort(variables)), 1L, 4L))
  }
  if (!is.null(suffix)) parts <- c(parts, suffix)
  paste(parts, collapse = "-")
}

#' Generate a default filter display name from its variables.
#'
#' Single-variable filters are named after their variable. Multi-variable
#' filters get a summarised name listing the first two variables, the count of
#' remaining variables, and the filter type, e.g.
#' `"age sex + 2 vars multi_discrete"`.
#'
#' @param variables Character vector of variable names.
#' @param type Filter type string (e.g. `"multi_discrete"`, `"query"`).
#' @return A single character string suitable for use as a filter name.
#' @keywords internal
.default_filter_name <- function(variables, type) {
  n <- length(variables)
  if (n <= 2L) {
    listed <- paste(variables, collapse = " ")
  } else {
    listed <- paste0(
      paste(variables[1:2], collapse = " "),
      " + ", n - 2L, " vars"
    )
  }
  paste(listed, type)
}

#' Get filter parameters as a list
#'
#' Extracts all user-facing properties from an S7 filter object.
#'
#' @param filter S7 filter object.
#' @param name Optional parameter name to retrieve a single value.
#' @return Named list of filter parameters, or a single value if `name` is given.
#'   Properties stored in `filter@private` are always excluded.
#' @export
get_filter_params <- function(filter, name) {
  all_props <- S7::props(filter)
  # Remove internal properties
  all_props$step_id <- NULL
  all_props$extra <- NULL
  all_props$private <- NULL
  # Merge extra params, excluding private keys
  extra <- filter@extra[setdiff(names(filter@extra), names(filter@private))]
  all_props <- c(all_props, extra)
  # description is returned as-is (may be NULL, character, or list)
  if (!missing(name)) return(all_props[[name]])
  all_props
}

#' Get a filter's domain
#'
#' Returns the declared domain (universe of valid values) attached to a filter,
#' or `NULL` when no domain is set. This is a thin accessor over the `domain`
#' property, intended for downstream consumers (e.g. GUIs) that render filter
#' inputs from the domain without reaching into S7 internals.
#'
#' @param filter S7 filter object.
#' @return The filter's domain, or `NULL` when unset. Structure depends on the
#'   filter type (e.g. a character vector for discrete filters, a two-element
#'   vector for range filters).
#' @seealso [filter_effective_value()], [cb_intersect_domain()]
#' @export
filter_domain <- function(filter) {
  filter@domain
}

#' Get a filter's effective value
#'
#' Returns the value that should be used to pre-select the filter input,
#' accounting for the domain. This is the filter's value intersected with its
#' domain; when the value is unset (`NA`) and a domain is present, the domain is
#' returned as the effective value. Thin wrapper over [cb_intersect_domain()]
#' for use by GUIs.
#'
#' @param filter S7 filter object.
#' @return The effective value for the filter, suitable for pre-selecting an
#'   input. Structure depends on the filter type.
#' @seealso [filter_domain()], [cb_intersect_domain()]
#' @export
filter_effective_value <- function(filter) {
  cb_intersect_domain(filter)
}

# -- discrete_text comma-separated helpers ------------------------------------

# The discrete_text filter represents its value, choices and domain as a single
# comma-separated string (e.g. "a, b, c") rather than a character vector. These
# helpers convert between that string form and a character vector, so all the
# discrete_text methods share one canonical splitting/joining rule. Splitting
# trims surrounding whitespace around every value (not just the first), so
# "a, b, c" yields c("a", "b", "c") and round-trips cleanly.

# Split a comma-separated discrete_text string into a character vector.
# `NA`/`NULL`/"" yield character(0). Whitespace around each value is trimmed and
# empty pieces are dropped, so values are matched exactly.
split_discrete_text <- function(x) {
  if (is.null(x) || identical(x, NA) || identical(x, "")) {
    return(character(0L))
  }
  pieces <- strsplit(as.character(x), split = ",", fixed = TRUE)[[1L]]
  pieces <- trimws(pieces)
  pieces[nzchar(pieces)]
}

# Join a character vector back into a canonical comma-separated string.
# An empty vector yields "" (the discrete_text "nothing selected" value).
join_discrete_text <- function(x) {
  paste(x, collapse = ",")
}

# -- Domain intersection -------------------------------------------------------

intersect_domain_discrete <- function(value, domain) {
  if (is.null(domain)) return(value)
  if (identical(value, NA)) return(domain)
  result <- intersect(value, domain)
  if (!identical(sort(as.character(value)), sort(as.character(result)))) {
    warning("Filter value trimmed to domain.", call. = FALSE)
  }
  result
}

# discrete_text variant: value and domain are comma-separated strings. Intersect
# them as sets of trimmed values and return a comma-separated string, so the
# effective value stays in the same string form the filter expects.
intersect_domain_discrete_text <- function(value, domain) {
  if (is.null(domain)) return(value)
  if (identical(value, NA)) return(domain)
  value_vec <- split_discrete_text(value)
  domain_vec <- split_discrete_text(domain)
  result_vec <- intersect(value_vec, domain_vec)
  if (!identical(sort(value_vec), sort(result_vec))) {
    warning("Filter value trimmed to domain.", call. = FALSE)
  }
  join_discrete_text(result_vec)
}

intersect_domain_range <- function(value, domain) {
  if (is.null(domain)) return(value)
  if (identical(value, NA)) return(domain)
  result <- c(max(value[1L], domain[1L]), min(value[2L], domain[2L]))
  if (!identical(value, result)) {
    warning("Filter value trimmed to domain.", call. = FALSE)
  }
  result
}

intersect_domain_multi <- function(values, domain) {
  if (is.null(domain)) return(values)
  if (identical(values, NA)) return(domain)
  result <- purrr::imap(values, function(val, nm) {
    if (nm %in% names(domain)) intersect(val, domain[[nm]]) else val
  })
  if (!identical(values, result)) {
    warning("Filter value trimmed to domain.", call. = FALSE)
  }
  result
}

#' Get effective filter value after domain intersection
#'
#' Returns the filter's value intersected with its domain. When value is `NA`
#' and domain is set, returns the domain as the effective value.
#'
#' Custom filter types should implement an S7 method for this generic.
#' The default method returns the raw value with no domain logic.
#'
#' @param filter S7 filter object.
#' @return The effective value for filtering.
#' @export
cb_intersect_domain <- S7::new_generic("cb_intersect_domain", "filter")

S7::method(cb_intersect_domain, CbFilter) <- function(filter) {
  S7::prop(filter, filter@private$input_param)
}

S7::method(cb_intersect_domain, CbFilterDiscrete) <- function(filter) {
  intersect_domain_discrete(filter@value, filter@domain)
}

S7::method(cb_intersect_domain, CbFilterDiscreteText) <- function(filter) {
  intersect_domain_discrete_text(filter@value, filter@domain)
}

S7::method(cb_intersect_domain, CbFilterRange) <- function(filter) {
  intersect_domain_range(filter@range, filter@domain)
}

S7::method(cb_intersect_domain, CbFilterDateRange) <- function(filter) {
  intersect_domain_range(filter@range, filter@domain)
}

S7::method(cb_intersect_domain, CbFilterDatetimeRange) <- function(filter) {
  intersect_domain_range(filter@range, filter@domain)
}

S7::method(cb_intersect_domain, CbFilterMultiDiscrete) <- function(filter) {
  intersect_domain_multi(filter@values, filter@domain)
}

S7::method(cb_intersect_domain, CbFilterQuery) <- function(filter) {
  filter@value
}

#' @keywords internal
intersect_domain <- function(filter) {
  cb_intersect_domain(filter)
}

#' Intersect two domain values for a filter type
#'
#' Combines two already-computed domain values (not value-vs-domain) into their
#' intersection, dispatching on filter type. Used by `"filter"`-mode domain
#' propagation when the same logical filter appears in more than one upstream
#' step and its effective domains must be combined. Unlike
#' [cb_intersect_domain()], this never emits trimming warnings.
#'
#' @param filter S7 filter object (used for type dispatch only).
#' @param a,b Domain values to intersect. Either may be `NULL`.
#' @return The intersected domain value, or `NULL`.
#' @export
cb_intersect_domain_values <- S7::new_generic("cb_intersect_domain_values", "filter")

S7::method(cb_intersect_domain_values, CbFilter) <- function(filter, a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  a
}

intersect_domain_values_discrete <- function(filter, a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  intersect(a, b)
}

S7::method(cb_intersect_domain_values, CbFilterDiscrete) <- intersect_domain_values_discrete

# discrete_text domains are comma-separated strings; intersect them as sets of
# trimmed values and return a comma-separated string.
intersect_domain_values_discrete_text <- function(filter, a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  join_discrete_text(intersect(split_discrete_text(a), split_discrete_text(b)))
}

S7::method(cb_intersect_domain_values, CbFilterDiscreteText) <- intersect_domain_values_discrete_text

intersect_domain_values_range <- function(filter, a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  c(max(a[1L], b[1L]), min(a[2L], b[2L]))
}

S7::method(cb_intersect_domain_values, CbFilterRange) <- intersect_domain_values_range
S7::method(cb_intersect_domain_values, CbFilterDateRange) <- intersect_domain_values_range
S7::method(cb_intersect_domain_values, CbFilterDatetimeRange) <- intersect_domain_values_range

S7::method(cb_intersect_domain_values, CbFilterMultiDiscrete) <- function(filter, a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  purrr::imap(a, function(val, nm) {
    if (nm %in% names(b)) intersect(val, b[[nm]]) else val
  })
}

#' Extract domain from cached filter statistics
#'
#' Derives a domain (set of valid values) from previously computed cache
#' statistics. Custom filter types should implement an S7 method for this
#' generic. The default method returns `NULL` (no domain).
#'
#' @param filter S7 filter object.
#' @param cache List of cached statistics for the filter.
#' @return Domain value appropriate for the filter type, or `NULL`.
#' @export
cb_domain_from_cache <- S7::new_generic("cb_domain_from_cache", "filter")

S7::method(cb_domain_from_cache, CbFilter) <- function(filter, cache) {
  NULL
}

S7::method(cb_domain_from_cache, CbFilterDiscrete) <- function(filter, cache) {
  if (is.null(cache$choices)) return(NULL)
  names(purrr::keep(cache$choices, ~ .x > 0L))
}

S7::method(cb_domain_from_cache, CbFilterDiscreteText) <- function(filter, cache) {
  # discrete_text caches `choices` as a single comma-separated string of the
  # distinct observed values (see cb_get_filter_stats), not a named count
  # vector. The domain is the same comma-separated string form as the filter's
  # value, so it round-trips through cb_intersect_domain().
  if (is.null(cache$choices)) return(NULL)
  join_discrete_text(split_discrete_text(cache$choices))
}

S7::method(cb_domain_from_cache, CbFilterRange) <- function(filter, cache) {
  if (!is.null(cache$min) && !is.null(cache$max)) c(cache$min, cache$max) else NULL
}

S7::method(cb_domain_from_cache, CbFilterDateRange) <- function(filter, cache) {
  if (!is.null(cache$min) && !is.null(cache$max)) c(cache$min, cache$max) else NULL
}

S7::method(cb_domain_from_cache, CbFilterDatetimeRange) <- function(filter, cache) {
  if (!is.null(cache$min) && !is.null(cache$max)) c(cache$min, cache$max) else NULL
}

S7::method(cb_domain_from_cache, CbFilterMultiDiscrete) <- function(filter, cache) {
  if (!is.null(cache$choices)) purrr::map(cache$choices, names) else NULL
}

#' Extract domain from data
#'
#' Derives a domain (set of valid values) directly from the data object.
#' Uses dual dispatch on filter type and source type. Custom filter types
#' should implement S7 methods for this generic. The default method returns
#' `NULL` (no domain).
#'
#' @param filter S7 filter object.
#' @param source Source object.
#' @param data_object Data object to extract domain from.
#' @param ... Additional arguments.
#' @return Domain value appropriate for the filter type, or `NULL`.
#' @export
cb_domain_from_data <- S7::new_generic("cb_domain_from_data", c("filter", "source"))

S7::method(cb_domain_from_data, list(CbFilter, S7::class_any)) <- function(filter, source, data_object, ...) {
  NULL
}

assign_filter_step_id <- function(filter_obj, step_id) {
  filter_obj@step_id <- step_id
  filter_obj
}

# -- Filter factory -----------------------------------------------------------

#' Define Cohort filter
#'
#' Creates an S7 filter object of the specified type.
#'
#' @param type Type of filter to use (e.g., "discrete", "range", "date_range").
#' @param ... Filter type-specific parameters.
#' @return An S7 filter object inheriting from `CbFilter`.
#'
#' @export
filter <- function(type, ...) {
  constructor <- switch(type,
    discrete = CbFilterDiscrete,
    discrete_text = CbFilterDiscreteText,
    range = CbFilterRange,
    date_range = CbFilterDateRange,
    datetime_range = CbFilterDatetimeRange,
    multi_discrete = CbFilterMultiDiscrete,
    query = CbFilterQuery,
    NULL
  )
  if (is.null(constructor)) {
    constructor <- .filter_registry[[type]]
  }
  if (is.null(constructor)) {
    stop(paste("Unknown filter type:", type))
  }
  constructor(...)
}

# -- Printing -----------------------------------------------------------------

#' Method for printing filter details
#'
#' @param filter The defined filter object.
#' @param data_objects List of data objects for the underlying filtering step.
#' @param to_string If `TRUE`, return the output as a character vector
#'   instead of printing it. Defaults to `FALSE`.
#' @export
.print_filter <- function(filter, data_objects, to_string = FALSE) {
  UseMethod(".print_filter", filter)
}

#' @rdname dot-print_filter
#' @export
.print_filter.default <- function(filter, data_objects, to_string = FALSE) {
  if (S7::S7_inherits(filter, CbFilterQuery)) {
    return(.print_filter.CbFilterQuery(filter, data_objects, to_string = to_string))
  }
  if (S7::S7_inherits(filter, CbFilter)) {
    return(.print_filter.CbFilter(filter, data_objects, to_string = to_string))
  }
  stop(
    "No applicable .print_filter method for class: ",
    paste(class(filter), collapse = ", "),
    call. = FALSE
  )
}

#' @export
.print_filter.CbFilter <- function(filter, data_objects, to_string = FALSE) {
  params <- get_filter_params(filter)
  params <- params[setdiff(names(params), static_params)]
  lines <- c(
    glue::glue("-> Filter ID: {filter@id}"),
    glue::glue("   Filter Type: {filter@type}"),
    "   Filter Parameters:"
  )
  for (param_name in names(params)) {
    lines <- c(lines, glue::glue("     {param_name}: {paste(params[[param_name]], collapse = ', ')}"))
  }
  if (to_string) return(lines)
  cat(lines, sep = "\n")
}

#' @export
.print_filter.CbFilterQuery <- function(filter, data_objects, to_string = FALSE) {
  params <- get_filter_params(filter)
  params <- params[setdiff(names(params), static_params)]
  lines <- c(
    glue::glue("-> Filter ID: {filter@id}"),
    glue::glue("   Filter Type: {filter@type}"),
    "   Filter Parameters:"
  )
  for (param_name in names(params)) {
    if (param_name == "value") {
      lines <- c(lines, glue::glue("     {param_name}: {deparse(queryBuilder::queryToExpr(params[[param_name]]))}"))
    } else {
      lines <- c(lines, glue::glue("     {param_name}: {paste(params[[param_name]], collapse = ', ')}"))
    }
  }
  if (to_string) return(lines)
  cat(lines, sep = "\n")
}

# -- Operators ----------------------------------------------------------------

#' Operator simplifying adding steps or filters to Cohort and Source objects
#'
#' When called with filter or step object, runs add_filter and add_step respectively.
#'
#' @param x Source or Cohort object. Otherwise works as a standard pipe operator.
#' @param object Filter or step to be added to `x`.
#' @return An object (`Source` or `Cohort`) having new filter or step added.
#'
#' @export
`%->%` <- function(x, object) {
  if (inherits(object, "cb_step")) {
    return(add_step(x, object))
  }
  if (S7::S7_inherits(object, CbFilter)) {
    return(add_filter(x, object))
  }
  object(x)
}
