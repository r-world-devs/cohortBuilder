#' R6 class representing a data source
#'
#' Source is an object storing information about data source such as source type,
#' primary keys and relations between stored data.
#' @export
Source <- R6::R6Class(
  "Source",
  public = list(
    #' @description
    #' Create a new `Source` object.
    #' @param dtconn An object defining source data connection.
    #' @param ... Extra Source parameters. Stored within `attributes` field.
    #' @param primary_keys Definition of data `primary_keys`, if appropriate. See \link{primary_keys}.
    #' @param binding_keys Definition of relations between data, if appropriate. See \link{binding-keys}.
    #' @param source_code A quote object that allows to recreate basic source structure.
    #'   Used as a part of reproducible code output, see \link{code}.
    #' @param description A named list storing the source objects description.
    #'   Can be accessed with \link{description} Cohort method.
    #' @param options List of options affecting methods output. Currently supported only `display_binding`
    #'   specifying whether reproducible code should include bindings definition.
    #' @return A new `Source` object of class `Source` (and `dtconn` object class appended).
    initialize = function(
      dtconn, ..., primary_keys = NULL, binding_keys = NULL, source_code = NULL,
      description = NULL,  options = list(display_binding = TRUE)
      ) {

      self$dtconn <- dtconn
      self$attributes <- list(...)
      self$source_code = source_code
      self$description <- description
      if (!is.null(binding_keys)) {
        self$binding_keys <- binding_keys
      }
      if (!is.null(primary_keys)) {
        self$primary_keys <- primary_keys
      }
      self$options <- options
      class(self) <- c(class(dtconn), class(self))
    },
    #' @description
    #' Get selected `Source` object `attribute`.
    #' @param param Name of the attribute.
    get = function(param) {
      self$attributes[[param]]
    },
    #' @description
    #' Returns filtering steps definition, if defined for `Source`.
    get_steps = function() {
      private$steps
    },
    #' @description
    #' Add filtering step definition.
    #' @param step Step definition created with \link{step}.
    add_step = function(step) {
      private$steps <- append(
        private$steps,
        stats::setNames(
          list(step),
          as.character(length(private$steps) + 1)
        )
      )
    },
    #' @description
    #' Remove filtering step definition.
    #' @param step_id Id of the step to be removed.
    rm_step = function(step_id) {
      if (missing(step_id)) {
        step_id <- length(private$steps)
      }
      step_id <- as.character(step_id)

      if (step_id == "0") {
        warning("No steps to remove or wrong ID passed")
      }

      private$steps[[step_id]] <- NULL
      if (length(private$steps) >= 1) {
        names(private$steps) <- as.character(seq_len(length(private$steps)))
      } else {
        private$steps <- NULL
      }
    },
    #' @description
    #' Add filter definition to selected step.
    #' @param filter Filter definition created with \link{filter}.
    #' @param step_id Id of the step to include the filter to.
    #'   If skipped the last step is used.
    add_filter = function(filter, step_id) {

      if (missing(step_id)) {
        step_id <- 1
        if (!is.null(private$steps)) {
          step_id <- length(private$steps)
        }
      }
      step_id <- as.character(step_id)

      if (is.null(private$steps[[step_id]])) {
        private$steps[[step_id]] <- step(filter)
      } else {
        private$steps[[step_id]]$filters <- append(
          private$steps[[step_id]]$filters,
          list(filter)
        )
      }
    },
    #' @description
    #' Remove filter definition from selected step.
    #' @param step_id Id of the step where filter is defined.
    #' @param filter_id Id of the filter to be removed.
    rm_filter = function(step_id, filter_id) {
      step_id <- as.character(step_id)
      to_remove_idx <- match_filter_id(
        private$steps[[step_id]]$filters,
        filter_id
      )

      private$steps[[step_id]]$filters[[to_remove_idx]] <- NULL

      if (length(private$steps[[step_id]]$filters) == 0) {
        private$steps[[step_id]] <- NULL # remove step when no more filters
      }

      if (length(private$steps) == 0) {
        private$steps <- NULL
      }
    },
    #' @description
    #' Update filter definition.
    #' @param step_id Id of the step where filter is defined.
    #' @param filter_id Id of the filter to be updated.
    #' @param ... Parameters with its new values.
    update_filter = function(step_id, filter_id, ...) {
      new_params <- list(...)
      step_id <- as.character(step_id)
      to_update_idx <- match_filter_id(
        private$steps[[step_id]]$filters,
        filter_id
      )

      for (param in names(new_params)) {
        environment(private$steps[[step_id]]$filters[[to_update_idx]])$args[[param]] <- new_params[[param]]
      }
    },
    #' @field dtconn Data connection object the Source if based on.
    dtconn = NULL,
    #' @field description Source object description list.
    description = NULL,
    #' @field attributes Extra source parameters passed when source is defined.
    attributes = list(),
    #' @field options Extra configuration options.
    options = list(),
    #' @field binding_keys Source data relations expressed as \link{binding-keys}.
    binding_keys = NULL,
    #' @field primary_keys Source data primary keys expressed as \link{primary_keys}.
    primary_keys = NULL,
    #' @field source_code An expression which allows to recreate basic source structure.
    source_code = NULL
  ),
  private = list(
    steps = NULL
  )
)

match_filter_id <- function(filters, filter_id) {
  filters_ids <- filters %>%
    purrr::map_chr(~ environment(.x)$id)
  which(filters_ids == filter_id)
}

check_layer <- function(dtconn) {

  if (inherits(dtconn, "tblist")) {
    return(invisible(TRUE))
  }
  layers <- paste0("set_source.", class(dtconn))

  layer_available <- FALSE
  for (layer in layers) {
    if (!is.null(.get_method(layer))) {
      layer_available <- TRUE
    }
  }
  if (!layer_available) {
    stop(
      "No extension found for the provided data connection type.
       Please try to install valid extension and load it."
    )
  }
  return(invisible(TRUE))
}

#' Create Cohort source
#'
#' Source is an object storing information about data source such as source type,
#' primary keys and relations between stored data.
#'
#' @param dtconn An object defining source data connection.
#' @param ... Source type specific parameters. Available in `attributes` list of resulting object.
#' @param primary_keys Definition of primary keys describing source data (if valid).
#'     When provided, affects the output of attrition data plot. See \link{primary_keys}.
#' @param binding_keys Definition of binding keys describing relations in source data (if valid).
#'     When provided, affects post filtering data. See \link{binding-keys}.
#' @param source_code Expression presenting low-level code for creating source.
#'     When provided, used as a part of reproducible code output.
#' @param description A named list storing the source objects description.
#'     Can be accessed with \link{description} Cohort method.
#' @examples
#' mtcars_source <- set_source(
#'   tblist(mtcars = mtcars),
#'   source_code = quote({
#'     source <- list(dtconn = list(datasets = mtcars))
#'   })
#' )
#' mtcars_source$attributes
#' @returns R6 object of class inherited from `dtconn`.
#' @export
set_source <- function(dtconn, ..., primary_keys = NULL, binding_keys = NULL,
                       source_code = NULL, description = NULL) {

  check_layer(dtconn)
  attr(dtconn, "call") <- rlang::call_match()$dtconn
  UseMethod("set_source", dtconn)
}

#' Source compatibility methods.
#'
#' @description
#' List of methods that allow compatibility of different source types.
#' Most of the methods should be defined in order to make new source layer functioning.
#' See 'Details' section for more information.
#'
#' @details
#' The package is designed to make the functionality work with multiple data sources.
#' Data source can be based for example on list of tables, connection to database schema
#' or API service that allows to access and operate on data.
#' In order to make new source type layer functioning, the following list of methods
#' should be defined:
#' \itemize{
#'   \item{\code{.init_source} - Defines how to extract data object from source.
#'   Each filtering step assumes to be operating on resulting data object
#'   (further named data_object) and returns object of the same type and structure.}
#'   \item{\code{.collect_data} - Defines how to collect data (into R memory) from `data_object`.}
#'   \item{\code{.get_stats} - Defines what `data_object` statistics should be
#'   calculated and how. When provided the stats can be extracted using \link{stat}.}
#'   \item{\code{.pre_filtering} - (optional) Defines what operation on `data_object` should be
#'   performed before applying filtering in the step.}
#'   \item{\code{.post_filtering} - (optional) Defines what operation on `data_object` should be
#'   performed after applying filtering in the step (before running binding).}
#'   \item{\code{.post_binding} - (optional) Defines what operation on `data_object` should be
#'   performed after applying binding in the step.}
#'   \item{\code{.run_binding} - (optional) Defines how to handle post filtering data binding.
#'   See more about binding keys at \link{binding-keys}.}
#'   \item{\code{.get_attrition_count and .get_attrition_label} - Methods defining how to
#'   get statistics and labels for attrition plot.}
#'   \item{\code{.repro_code_tweak} - (optional) Default method passed as a `modifier`
#'   argument of \link{code} function. Aims to modify reproducible code into the final format.}
#' }
#' Except from the above methods, you may extend the existing or new source with providing
#' custom filtering methods. See \link{creating-filters}.
#' In order to see more details about how to implement custom source check `vignette("custom-extensions")`.
#'
#' @name source-layer
#' @param source Source object.
#' @param data_object Object that allows source data access.
#'     `data_object` is the result of `.init_step` method (or object of the same structure).
#' @param step_id Id of the filtering step.
#' @param code_data Data frame storing `type`, `expr` and filter or step related columns.
#' @return Depends on specific method. See `vignette("custom-extensions")` for more details.
NULL

#' @rdname source-layer
#' @export
.init_step <- function(source, ...) {
  UseMethod(".init_step", source)
}

#' @rdname source-layer
#' @export
.init_step.default <- function(source, ...) {
  return(NULL)
}

#' @rdname source-layer
#' @export
.collect_data <- function(source, data_object) {
  UseMethod(".collect_data", source)
}

#' @rdname source-layer
#' @export
.collect_data.default <- function(source, data_object) {
  return(NULL)
}

#' @rdname source-layer
#' @export
.get_stats <- function(source, data_object) {
  UseMethod(".get_stats", source)
}

#' @rdname source-layer
#' @export
.get_stats.default <- function(source, data_object) {
  return(NULL)
}

#' @rdname source-layer
#' @export
.pre_filtering <- function(source, data_object, step_id) {
  UseMethod(".pre_filtering", source)
}

#' @rdname source-layer
#' @export
.post_filtering <- function(source, data_object, step_id) {
  UseMethod(".post_filtering", source)
}

#' @rdname source-layer
#' @export
.post_binding <- function(source, data_object, step_id) {
  UseMethod(".post_binding", source)
}

#' @rdname source-layer
#' @export
.repro_code_tweak <- function(source, code_data) {
  UseMethod(".repro_code_tweak", source)
}

#' @title Managing the Source object
#'
#' @description
#' The list of methods designed for managing the Source configuration and state.
#'
#' \itemize{
#'    \item{\link{add_step} - Add step to Source object.}
#'    \item{\link{rm_step} - Remove step from Source object.}
#'    \item{\link{add_filter} - Add filter to Source step.}
#'    \item{\link{rm_filter} - Remove filter from Source step.}
#'    \item{\link{update_filter} - Update filter configuration.}
#' }
#'
#' @name managing-source
#' @return The object of class `Source` having the modified configuration dependent on the used method.
#' @seealso managing-cohort
NULL

#' @rdname source-layer
#' @export
.pre_filtering.default <- function(source, data_object, step_id) {
  return(data_object)
}

#' @rdname source-layer
#' @export
.post_filtering.default <- function(source, data_object, step_id) {
  return(data_object)
}

#' @rdname source-layer
#' @export
.post_binding.default <- function(source, data_object, step_id) {
  return(data_object)
}

#' @rdname add_step
#' @export
add_step.Source <- function(x, step, ...) {
  x$add_step(step)
  return(x)
}

#' @rdname rm_step
#' @export
rm_step.Source <- function(x, step_id, ...) {
  x$rm_step(step_id)
  return(x)
}

#' @rdname add_filter
#' @export
add_filter.Source <- function(x, filter, step_id, ...) {
  x$add_filter(filter, step_id)
  return(x)
}

#' @rdname rm_filter
#' @export
rm_filter.Source <- function(x, step_id, filter_id, ...) {
  x$rm_filter(step_id, filter_id)
  return(x)
}

#' @rdname update_filter
#' @export
update_filter.Source <- function(x, step_id, filter_id, ...) {
  x$update_filter(step_id, filter_id, ...)
  return(x)
}
