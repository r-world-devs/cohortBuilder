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
#'   properties = list(variable = S7::class_character),
#'   constructor = function(id = .gen_id(), name = id, variable, dataset, ...) {
#'     S7::new_object(S7::S7_object(),
#'       type = "my_filter", id = id, name = name, input_param = "value",
#'       variable = variable, dataset = dataset,
#'       active = TRUE, description = NULL, extra = list(...)
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
#' @param input_param Name of the parameter holding the filtering value.
#' @param dataset Dataset name to apply the filter on.
#' @param active Whether the filter is active.
#' @param description Optional filter description.
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
    input_param = S7::class_character,
    dataset = S7::class_character,
    active = S7::class_logical,
    description = S7::class_any,
    step_id = S7::new_property(S7::class_any, default = NULL),
    extra = S7::new_property(S7::class_any, default = list())
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
    variable = S7::class_character,
    value = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = .gen_id(), name = id, variable, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    S7::new_object(S7::S7_object(),
      type = "discrete", id = id, name = name, input_param = "value",
      variable = variable, value = value, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      extra = list(...)
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
    variable = S7::class_character,
    value = S7::class_any,
    keep_na = S7::new_property(S7::class_logical, default = TRUE)
  ),
  constructor = function(id = .gen_id(), name = id, variable, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    S7::new_object(S7::S7_object(),
      type = "discrete_text", id = id, name = name, input_param = "value",
      variable = variable, value = value, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      extra = list(...)
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
    variable = S7::class_character,
    range = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = .gen_id(), name = id, variable, range = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    S7::new_object(S7::S7_object(),
      type = "range", id = id, name = name, input_param = "range",
      variable = variable, range = range, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      extra = list(...)
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
    variable = S7::class_character,
    range = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = .gen_id(), name = id, variable, range = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    S7::new_object(S7::S7_object(),
      type = "date_range", id = id, name = name, input_param = "range",
      variable = variable, range = range, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      extra = list(...)
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
    variable = S7::class_character,
    range = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = .gen_id(), name = id, variable, range = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    S7::new_object(S7::S7_object(),
      type = "datetime_range", id = id, name = name, input_param = "range",
      variable = variable, range = range, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      extra = list(...)
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
    variables = S7::class_any,
    values = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = .gen_id(), name = id, values, variables,
                         dataset, keep_na = TRUE, description = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    S7::new_object(S7::S7_object(),
      type = "multi_discrete", id = id, name = name, input_param = "values",
      variables = variables, values = values, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      extra = list(...)
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
    variables = S7::class_any,
    value = S7::class_any,
    keep_na = S7::class_logical
  ),
  constructor = function(id = .gen_id(), name = id, variables, value = NA,
                         dataset, keep_na = TRUE, description = NULL,
                         active = getOption("cb_active_filter", default = TRUE), ...) {
    S7::new_object(S7::S7_object(),
      type = "query", id = id, name = name, input_param = "value",
      variables = variables, value = value, dataset = dataset,
      keep_na = keep_na, active = active, description = description,
      extra = list(...)
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
#' @export
get_filter_params <- function(filter, name) {
  all_props <- S7::props(filter)
  # Remove internal properties
  all_props$step_id <- NULL
  all_props$extra <- NULL
  all_props$input_param <- NULL
  # Merge extra params
  all_props <- c(all_props, filter@extra)
  # description is returned as-is (may be NULL, character, or list)
  if (!missing(name)) return(all_props[[name]])
  all_props
}

get_filter_state <- function(filter, extra_fields) {
  params <- get_filter_params(filter)
  # Remove gui closures attached by shinyCohortBuilder
  params$gui <- NULL
  if (!is.null(extra_fields)) {
    for (field in extra_fields) {
      params[[field]] <- S7::prop(filter, field)
    }
  }
  params
}

eval_filter <- function(filter_obj, step_id, source) {
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
#' @export
.print_filter <- function(filter, data_objects) {
  UseMethod(".print_filter", filter)
}

#' @rdname dot-print_filter
#' @export
.print_filter.default <- function(filter, data_objects) {
  if (S7::S7_inherits(filter, CbFilterQuery)) {
    return(.print_filter.CbFilterQuery(filter, data_objects))
  }
  if (S7::S7_inherits(filter, CbFilter)) {
    return(.print_filter.CbFilter(filter, data_objects))
  }
  stop(
    "No applicable .print_filter method for class: ",
    paste(class(filter), collapse = ", "),
    call. = FALSE
  )
}

#' @export
.print_filter.CbFilter <- function(filter, data_objects) {
  params <- get_filter_params(filter)
  params <- params[setdiff(names(params), static_params)]
  cat(glue::glue("-> Filter ID: {filter@id}"), sep = "\n")
  cat(glue::glue("   Filter Type: {filter@type}"), sep = "\n")
  cat("   Filter Parameters:", sep = "\n")
  for (param_name in names(params)) {
    cat(glue::glue("     {param_name}: {paste(params[[param_name]], collapse = ', ')}"), sep = "\n")
  }
}

#' @export
.print_filter.CbFilterQuery <- function(filter, data_objects) {
  params <- get_filter_params(filter)
  params <- params[setdiff(names(params), static_params)]
  cat(glue::glue("-> Filter ID: {filter@id}"), sep = "\n")
  cat(glue::glue("   Filter Type: {filter@type}"), sep = "\n")
  cat("   Filter Parameters:", sep = "\n")
  for (param_name in names(params)) {
    if (param_name == "value") {
      cat(glue::glue("     {param_name}: {deparse(queryBuilder::queryToExpr(params[[param_name]]))}"), sep = "\n")
    } else {
      cat(glue::glue("     {param_name}: {paste(params[[param_name]], collapse = ', ')}"), sep = "\n")
    }
  }
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
