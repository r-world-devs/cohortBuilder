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
#'   constructor = function(id = .gen_id(), name = id, variable, dataset,
#'                          description = NULL, domain = NULL, ...) {
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
  constructor = function(id = .gen_id(), name = id, variable, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
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
  constructor = function(id = .gen_id(), name = id, variable, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
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
  constructor = function(id = .gen_id(), name = id, variable, range = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
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
  constructor = function(id = .gen_id(), name = id, variable, range = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
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
  constructor = function(id = .gen_id(), name = id, variable, range = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
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
  constructor = function(id = .gen_id(), name = id, values, variables,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
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
  constructor = function(id = .gen_id(), name = id, variables, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         domain = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
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
#' @param filter S7 filter object.
#' @return The effective value for filtering.
#' @keywords internal
intersect_domain <- function(filter) {
  input_param <- filter@private$input_param
  value <- S7::prop(filter, input_param)
  domain <- filter@domain

  switch(filter@type,
    discrete = ,
    discrete_text = intersect_domain_discrete(value, domain),
    range = ,
    date_range = ,
    datetime_range = intersect_domain_range(value, domain),
    multi_discrete = intersect_domain_multi(value, domain),
    query = value,
    value
  )
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
