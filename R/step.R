#' Retrieve the steps configured on a source
#'
#' @param source A `Source` object.
#' @param ... Ignored.
#' @return The source's list of steps.
#' @noRd
get_steps <- function(source, ...) {
  source$get_steps()
}

#' Test whether a source has any steps configured
#'
#' @param source A `Source` object.
#' @return `TRUE` when the source has at least one step.
#' @noRd
has_steps <- function(source) {
  !is.null(source$get_steps())
}

#' Normalize varied step inputs into a list of `cb_step` objects
#'
#' Accepts a single step, a single filter, a list of steps, or loose filters and
#' returns a consistent list of `cb_step` objects.
#'
#' @param steps A list of steps and/or filters.
#' @return A list of `cb_step` objects.
#' @noRd
structure_steps <- function(steps) {

  if (length(steps) == 1L) {
    single_step <- "cb_step" %in% class(steps[[1L]])
    if (single_step) {
      return(steps)
    }
    return(list(step(steps[[1L]])))
  } else {
    is_list_of_steps <- all(purrr::map_lgl(steps, ~ "cb_step" %in% class(.)))
    if (is_list_of_steps) {
      return(steps)
    }
    return(list(do.call(step, steps)))
  }

  steps
}

#' Collect and id-assign steps from a source or loose arguments
#'
#' Resolves steps from `source` (if it has any) or from `...`, normalizes them
#' via [structure_steps()], and assigns sequential step ids.
#'
#' @param source A `Source` object (optional).
#' @param ... Steps and/or filters used when the source has none.
#' @return A named list of `cb_step` objects, or `NULL` when there is nothing.
#' @noRd
pull_steps <- function(source, ...) {
  if (missing(source) || (!has_steps(source) && length(list(...)) == 0L)) {
    return(NULL)
  } else if (has_steps(source)) { # steps or raw filters are added as source attributes
    steps <- get_steps(source, ...)
  } else {
    steps <- list(...)
  }

  steps |>
    structure_steps() |>
    assign_step_ids()
}

#' Assign a step id to each of a step's filters and name them
#'
#' Stamps `step_id` onto each filter, names the filters by their ids, and errors
#' if two filters in the step share an id.
#'
#' @param step A `cb_step` object.
#' @return The step with named, id-stamped filters.
#' @noRd
assign_filters_to_step <- function(step) {

  if (length(step$filters) == 0L) {
    return(list())
  }

  step$filters <- step$filters |>
    purrr::map(assign_filter_step_id, step_id = step$id)

  filters_names <- step$filters |> purrr::map_chr(~.x@id)
  if (anyDuplicated(filters_names) > 0L) {
    stop("Cannot create filters with the same id in a single step.")
  }
  step$filters <- step$filters |>
    stats::setNames(filters_names)

  return(step)
}

#' Build the full step/filter structure for a source
#'
#' Combines [pull_steps()] and [assign_filters_to_step()] to produce steps with
#' ids assigned to both steps and their filters.
#'
#' @param source A `Source` object (optional).
#' @param ... Steps and/or filters.
#' @return A named list of fully id-assigned `cb_step` objects.
#' @noRd
register_steps_and_filters <- function(source, ...) {

  steps <- pull_steps(source = source, ...) |>
    purrr::map(assign_filters_to_step)

  return(steps)
}

#' Set the id of a single step
#'
#' @param step A `cb_step` object.
#' @param id Step id to assign.
#' @return The step with `id` set.
#' @noRd
assign_step_id <- function(step, id) {
  step$id <- id
  return(step)
}

#' Assign sequential ids to a list of steps
#'
#' @param steps A list of `cb_step` objects.
#' @return The list named `"1"`, `"2"`, ... with matching `id` fields.
#' @noRd
assign_step_ids <- function(steps) {
  step_ids <- as.character(seq_along(steps))
  steps |>
    purrr::imodify(~ assign_step_id(.x, as.character(.y))) |>
    stats::setNames(step_ids)
}

#' Inclusive sequence of step ids between two bounds
#'
#' @param from,to Step ids (coerced to integer).
#' @return A character vector of step ids, empty when `from > to`.
#' @noRd
steps_range <- function(from, to) {
  from <- as.integer(from)
  to <- as.integer(to)
  if (from > to) {
    return(character(0L))
  }
  as.character(
    seq(from = from, to = to, by = 1L)
  )
}

#' Re-id a step and propagate the new id to its filters
#'
#' @param step A `cb_step` object.
#' @param new_id The new step id.
#' @return The step with `id` and each filter's `step_id` updated.
#' @noRd
readjust_step <- function(step, new_id) {
  step$id <- new_id
  step$filters <- purrr::modify(step$filters, function(f) { f@step_id <- new_id; f })

  return(step)
}

#' Step id immediately before `idx`
#'
#' @param idx A step id.
#' @return The preceding step id as a character string.
#' @noRd
prev_step <- function(idx) {
  as.character(as.integer(idx) - 1L)
}

#' Step id immediately after `idx`
#'
#' @param idx A step id.
#' @return The following step id as a character string.
#' @noRd
next_step <- function(idx) {
  as.character(as.integer(idx) + 1L)
}

#' Print or render a step and its filters
#'
#' @param step A `cb_step` object.
#' @param to_string If `TRUE`, return character lines instead of printing.
#' @return Character lines (when `to_string = TRUE`) or `step` invisibly.
#' @noRd
print_step <- function(step, to_string = FALSE) {
  pending_flag <- if (isTRUE(step$pending)) " [pending]" else ""
  header <- glue::glue(">> Step ID: {step$id}{pending_flag}")
  if (to_string) {
    filter_lines <- step$filters |>
      purrr::map(~ .print_filter(.x, data_objects = NULL, to_string = TRUE)) |>
      unlist()
    return(c(as.character(header), filter_lines))
  }
  cat(header, sep = "\n")
  step$filters |>
    purrr::walk(.print_filter, data_objects = NULL)
}

#' Create filtering step
#'
#' Steps all to perform multiple stages of Source data filtering.
#'
#' @examples
#' iris_step_1 <- step(
#'   filter('discrete', dataset = 'iris', variable = 'Species', value = 'setosa'),
#'   filter('discrete', dataset = 'iris', variable = 'Petal.Length', range = c(1.5, 2))
#' )
#' iris_step_2 <- step(
#'   filter('discrete', dataset = 'iris', variable = 'Sepal.Length', range = c(5, 10))
#' )
#'
#' # Add step directly to Cohort
#' iris_source <- set_source(tblist(iris = iris))
#' coh <- iris_source |>
#'   cohort(
#'     iris_step_1,
#'     iris_step_2
#'   ) |>
#'   run()
#'
#' nrow(get_data(coh, step_id = 1)$iris)
#' nrow(get_data(coh, step_id = 2)$iris)
#'
#' # Add step to Cohort using add_step method
#' coh <- iris_source |>
#'   cohort()
#' coh <- coh |>
#'   add_step(iris_step_1) |>
#'   add_step(iris_step_2) |>
#'   run()
#'
#' @param ... Filters. See \link{filter}.
#' @return List of class `cb_step` storing filters configuration.
#'
#' @export
step <- function(...) {
  structure(
    list(filters = list(...)),
    class = "cb_step"
  )
}
