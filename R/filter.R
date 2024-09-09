static_params <- c("type", "id", "name")

eval_filter <- function(filter_fun, step_id, source) {
  evaled_filter <- filter_fun(source)
  evaled_filter$step_id <- step_id

  evaled_filter
}

#' Generate random ID
#'
#' @return A character type value.
#' @export
.gen_id <- function() {
  paste0(
    paste0(sample(LETTERS, 5, TRUE), collapse = ""),
    round(as.numeric(Sys.time()) * 1000)
  )
}

get_filter_state <- function(filter, extra_fields) {
  filter_params <- as.list(environment(filter$filter_data))
  filter_params <- append(
    filter_params,
    filter_params$args
  )
  filter_params$args <- NULL
  filter_params$source <- NULL
  filter_params$type <- as.character(filter_params$type)

  if (!is.null(extra_fields)) {
    for (field in extra_fields) {
      filter_params[[field]] <- filter[[field]]
    }
  }

  return(filter_params)
}

#' Define custom filter.
#'
#' Methods available for creating new filters easier.
#'
#' `def_filter` designates list of parameters and methods required to define new type of filter.
#'
#' `new_filter` creates a new file with new filter definition template.
#'
#' See vignettes("custom-filters") to learn how to create a custom filter.
#'
#' @name creating-filters
#' @param type Filter type.
#' @param id Filter id.
#' @param name Filter name.
#' @param input_param Name of parameter responsible for providing filtering value.
#' @param filter_data Function of `data_object` parameter defining filtering logic on Source data object.
#' @param get_stats Function of `data_object` and `name` parameters defining what and how data statistics should be calculated.
#' @param plot_data Function of `data_object` parameter defining how filter data should be plotted.
#' @param get_params Function of `name` parameter returning filter parameters (if names is skipped all the parameters are returned).
#' @param get_data Function of `data_object` returning filter related data.
#' @param get_defaults Function of `data_object` and `cache_object` parameters returning default `input_param` parameter value.
#' @return A list of filter specific values and methods (`def_filter`) or no value (`new_filter`).
#'
#' @export
def_filter <- function(type, id = .gen_id(), name = id, input_param = NULL,
  filter_data, get_stats, plot_data, get_params, get_data, get_defaults) {

  structure(
    list(
      id = id,
      type = type,
      name = name,
      input_param = input_param,
      filter_data = filter_data,
      get_stats = get_stats,
      plot_data = plot_data,
      get_params = get_params,
      get_data = get_data,
      get_defaults = get_defaults
    ),
    class = c("cb_filter", type)
  )
}

#' @rdname creating-filters
#' @param filter_type Type of new filter.
#' @param source_type Type of source for which filter should be defined.
#' @param input_param Name of the parameter taking filtering value.
#' @param extra_params Vector of extra parameters name that should be available for filter.
#' @param file File path where filter should be created.
#' @export
new_filter <- function(filter_type, source_type, input_param = "value", extra_params = "", file) {
  template_content <- as.list(c(
    readLines(system.file("filter_template", package = "cohortBuilder")),
    .sep = "\n"
  ))
  extra_params_assign <- ""
  if (!identical(extra_params, "")) {
    extra_params_assign <- paste0(paste(
      glue::glue("{extra_params} = {extra_params}"),
      collapse = ", "
    ), ",")
    extra_params <- paste0(paste(extra_params, collapse = ", "), ",")
  }
  file = file.path(getwd(), glue::glue("filter_{filter_type}_{source_type}.R"))
  writeLines(
    do.call(glue::glue, as.list(template_content)),
    con = file
  )
  utils::file.edit(file)
}

#' Method for printing filter details
#'
#' @param filter The defined filter object.
#' @param data_objects List of data objects for the underlying filtering step.
#' @export
.print_filter <- function(filter, data_objects) {
  UseMethod(".print_filter", filter)
}

#' @export
.print_filter.default <- function(filter, data_objects) {
  meta <- filter$get_params()
  params <- meta[setdiff(names(meta), static_params)]
  cat(glue::glue("-> Filter ID: {filter$id}"), sep = "\n")
  cat(glue::glue("   Filter Type: {filter$type}"), sep = "\n")
  cat("   Filter Parameters:", sep = "\n")
  for (param_name in names(params)) {
    cat(glue::glue("     {param_name}: {paste(params[[param_name]], collapse = ', ')}"), sep = "\n")
  }
}

#' Operator simplifying adding steps or filters to Cohort and Source objects
#'
#' When called with filter or step object, runs add_filter and add_step respectively.
#'
#' @param x Source or Cohort object. Otherwise works as a standard pipe operator.
#' @param object Filter or step to be added to `x`.
#' @return And object (`Source` or `Cohort`) having new filter of step added.
#'
#' @export
`%->%` <- function(x, object) {
  method <- NULL
  if (inherits(object, "cb_step")) {
    method <- add_step
  }
  if (inherits(object, "cb_filter_constructor")) {
    method <- add_filter
  }
  if (is.null(method)) {
    method <- `%>%`
  }
  return(method(x, object))
}

#' Attach proper class to filter constructor
#'
#' @param filter_constructor Function defining filter.
#' @return A function having `cb_filter_constructor` class attached.
#'
#' @export
.as_constructor <- function(filter_constructor) {
  class(filter_constructor) <- c(class(filter_constructor), "cb_filter_constructor")
  return(filter_constructor)
}

#' Define Cohort filter
#'
#' @param type Type of filter to use.
#' @param ... Filter type-specific parameters (see \link{filter-types}),
#'   and filter source-specific parameters (see \link{filter-source-types}).
#' @return A function of class `cb_filter_constructor`.
#'
#' @export
filter <- function(type, ...) {
  UseMethod("filter", type)
}

#' @rdname filter
#' @export
filter.character <- function(type, ...) {
  base_filter <- structure(
    type,
    class = c(type, "cb_filter")
  )
  filter(base_filter, ...)
}

#' Filter types
#'
#' @name filter-types
#' @param type Character string defining filter type (having class of the same value as type).
#' @param id Id of the filter.
#' @param name Filter name.
#' @param active If FALSE filter will be skipped during Cohort filtering.
#' @param description Filter description object. Preferable a character value.
#' @param ... Source specific parameters passed to filter (see \link{filter-source-types}).
#' @return A function of class `cb_filter_constructor`.
NULL

#' Filter Source types methods
#'
#' @inheritParams filter-types
#' @name filter-source-types
#' @param source Source object.
#' @param ... Source type specific parameters (or extra ones if not matching specific S3 method arguments).
#' @return List of filter-specific metadata and methods - result of evaluation of
#'    `cb_filter_constructor` function on `Source` object.
NULL

#' @rdname filter-types
#' @export
filter.discrete <- function(type, id, name, ..., active = getOption("cb_active_filter", default = TRUE)) {
  args <- append(
    environment() %>% as.list() %>% purrr::keep(~ !is.symbol(.x)),
    list(...)
  )

  .as_constructor(
    function(source) {
      do.call(
        cb_filter.discrete,
        append(list(source = source), args)
      )
    }
  )
}

#' @rdname filter-source-types
#' @export
cb_filter.discrete <- function(source, ...) {
  UseMethod("cb_filter.discrete", source)
}

#' @rdname filter-types
#' @export
filter.discrete_text <- function(type, id, name, ..., description = NULL,
                                 active = getOption("cb_active_filter", default = TRUE)) {
  args <- append(
    environment() %>% as.list() %>% purrr::keep(~ !is.symbol(.x)),
    list(...)
  )

  .as_constructor(
    function(source) {
      do.call(
        cb_filter.discrete_text,
        append(list(source = source), args)
      )
    }
  )
}

#' @rdname filter-source-types
#' @export
cb_filter.discrete_text <- function(source, ...) {
  UseMethod("cb_filter.discrete_text", source)
}

#' @rdname filter-types
#' @export
filter.range <- function(type, id, name, ..., description = NULL,
                         active = getOption("cb_active_filter", default = TRUE)) {
  args <- append(
    environment() %>% as.list() %>% purrr::keep(~ !is.symbol(.x)),
    list(...)
  )

  .as_constructor(
    function(source) {
      do.call(
        cb_filter.range,
        append(list(source = source), args)
      )
    }
  )
}

#' @rdname filter-source-types
#' @export
cb_filter.range <- function(source, ...) {
  UseMethod("cb_filter.range", source)
}

#' @rdname filter-types
#' @export
filter.date_range <- function(type, id, name, ..., description = NULL,
                              active = getOption("cb_active_filter", default = TRUE)) {
  args <- append(
    environment() %>% as.list() %>% purrr::keep(~ !is.symbol(.x)),
    list(...)
  )

  .as_constructor(
    function(source) {
      do.call(
        cb_filter.date_range,
        append(list(source = source), args)
      )
    }
  )
}

#' @rdname filter-source-types
#' @export
cb_filter.date_range <- function(source, ...) {
  UseMethod("cb_filter.date_range", source)
}

#' @rdname filter-types
#' @export
filter.multi_discrete <- function(type, id, name, ..., description = NULL,
                                  active = getOption("cb_active_filter", default = TRUE)) {
  args <- append(
    environment() %>% as.list() %>% purrr::keep(~ !is.symbol(.x)),
    list(...)
  )

  .as_constructor(
    function(source) {
      do.call(
        cb_filter.multi_discrete,
        append(list(source = source), args)
      )
    }
  )
}

#' @rdname filter-source-types
#' @export
cb_filter.multi_discrete <- function(source, ...) {
  UseMethod("cb_filter.multi_discrete", source)
}

#' @rdname filter-types
#' @export
filter.query <- function(type, id, name, ..., active = getOption("cb_active_filter", default = TRUE)) {
  args <- append(
    environment() %>% as.list() %>% purrr::keep(~ !is.symbol(.x)),
    list(...)
  )

  .as_constructor(
    function(source) {
      do.call(
        cb_filter.query,
        append(list(source = source), args)
      )
    }
  )
}

#' @rdname filter-source-types
#' @export
cb_filter.query <- function(source, ...) {
  UseMethod("cb_filter.query", source)
}

#' @export
.print_filter.query <- function(filter, data_objects) {
  meta <- filter$get_params()
  params <- meta[setdiff(names(meta), static_params)]
  cat(glue::glue("-> Filter ID: {filter$id}"), sep = "\n")
  cat(glue::glue("   Filter Type: {filter$type}"), sep = "\n")
  cat("   Filter Parameters:", sep = "\n")
  for (param_name in names(params)) {
    if (param_name == "value") {
      cat(glue::glue("     {param_name}: {deparse(queryBuilder::queryToExpr(params[[param_name]]))}"), sep = "\n")
    } else {
      cat(glue::glue("     {param_name}: {paste(params[[param_name]], collapse = ', ')}"), sep = "\n")
    }
  }
}

