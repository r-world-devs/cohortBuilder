get_steps <- function(source, ...) {
  source$get_steps()
}

has_steps <- function(source) {
  !is.null(source$get_steps())
}

structure_steps <- function(steps) {

  if (length(steps) == 1) {
    single_step <- "cb_step" %in% class(steps[[1]])
    if (single_step) {
      return(steps)
    }
    return(list(step(steps[[1]])))
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
  if (missing(source) || (!has_steps(source) && length(list(...)) == 0)) {
    return(NULL)
  } else if (has_steps(source)) { # steps or raw filters are added as source attributes
    steps <- get_steps(source, ...)
  } else {
    steps <- list(...)
  }

  steps %>%
    structure_steps() %>%
    attach_step_ids()
}

eval_step_filters <- function(step, source) {

  if (length(step$filters) == 0) {
    return(list())
  }

  step$filters <- step$filters %>%
    purrr::map(eval_filter, step_id = step$id, source = source)

  filters_names <- step$filters %>% purrr::map_chr(~.x$id)
  if (any(duplicated(filters_names))) {
    stop("Cannot create filters with the same id in a single step.")
  }
  step$filters <- step$filters %>%
    stats::setNames(filters_names)
  return(step)
}

register_steps_and_filters <- function(source, ...) {

  steps <- pull_steps(source = source, ...) %>%
    purrr::map(eval_step_filters, source = source)

  return(steps)
}

attach_step_id <- function(step, id) {
  step$id <- id
  return(step)
}

attach_step_ids <- function(steps) {
  step_ids <- as.character(seq_len(length(steps)))
  steps %>%
    purrr::imodify(~ attach_step_id(.x, as.character(.y))) %>%
    stats::setNames(step_ids)
}

steps_range <- function(from, to) {
  from <- as.integer(from)
  to <- as.integer(to)
  if (from > to) {
    return(character(0))
  }
  as.character(
    seq(from = from, to = to, by = 1)
  )
}

readjust_step <- function(step, new_id) {
  step$id <- new_id
  step$filters <- purrr::modify(step$filters, modify_item, new_val = new_id, what = "step_id")

  return(step)
}

prev_step <- function(idx) {
  as.character(as.integer(idx) - 1)
}

next_step <- function(idx) {
  as.character(as.integer(idx) + 1)
}

print_step <- function(step) {
  cat(glue::glue(">> Step ID: {step$id}"), sep = "\n")
  step$filters %>%
    purrr::walk(.print_filter, data_objects = NULL)
}

#' Create filtering step
#'
#' Steps all to perform multiple stages of Source data filtering.
#'
#' @examples
#' library(magrittr)
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
#' coh <- iris_source %>%
#'   cohort(
#'     iris_step_1,
#'     iris_step_2
#'   ) %>%
#'   run()
#'
#' nrow(get_data(coh, step_id = 1)$iris)
#' nrow(get_data(coh, step_id = 2)$iris)
#'
#' # Add step to Cohort using add_step method
#' coh <- iris_source %>%
#'   cohort()
#' coh <- coh %>%
#'   add_step(iris_step_1) %>%
#'   add_step(iris_step_2) %>%
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
