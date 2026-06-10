get_steps <- function(source, ...) {
  source$get_steps()
}

has_steps <- function(source) {
  !is.null(source$get_steps())
}

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

register_steps_and_filters <- function(source, ...) {

  steps <- pull_steps(source = source, ...) |>
    purrr::map(assign_filters_to_step)

  return(steps)
}

assign_step_id <- function(step, id) {
  step$id <- id
  return(step)
}

assign_step_ids <- function(steps) {
  step_ids <- as.character(seq_along(steps))
  steps |>
    purrr::imodify(~ assign_step_id(.x, as.character(.y))) |>
    stats::setNames(step_ids)
}

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

readjust_step <- function(step, new_id) {
  step$id <- new_id
  step$filters <- purrr::modify(step$filters, function(f) { f@step_id <- new_id; f })

  return(step)
}

prev_step <- function(idx) {
  as.character(as.integer(idx) - 1L)
}

next_step <- function(idx) {
  as.character(as.integer(idx) + 1L)
}

print_step <- function(step) {
  cat(glue::glue(">> Step ID: {step$id}"), sep = "\n")
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
    list(filters = list(...), cache = NULL),
    class = "cb_step"
  )
}
