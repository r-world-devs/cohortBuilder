# -- cb_tool S3 class -----------------------------------------------------------

#' Create a cohortBuilder tool definition
#'
#' Constructs a tool object that can be registered with an LLM chat via
#' \code{\link{cb_register_tool}}.
#'
#' @param fun Function to invoke when the tool is called.
#' @param name Character string identifying the tool.
#' @param description Character string describing what the tool does.
#'   The more detail provided, the better the LLM can decide when to use it.
#' @param arguments Named list of argument type definitions created by
#'   \pkg{ellmer} \code{type_*()} functions (e.g. \code{ellmer::type_string()}).
#' @return An object of class \code{cb_tool}.
#'
#' @export
cb_tool <- function(fun, name, description, arguments = list()) {
  structure(
    list(
      fun = fun,
      name = name,
      description = description,
      arguments = arguments
    ),
    class = "cb_tool"
  )
}

#' @rdname cb_tool
#' @param x A \code{cb_tool} object.
#' @param ... Ignored.
#' @export
print.cb_tool <- function(x, ...) {
  cat("cohortBuilder tool:", x$name, "\n")
  cat("Description:", trimws(x$description), "\n")
  if (length(x$arguments) > 0L) {
    cat("Arguments:", paste(names(x$arguments), collapse = ", "), "\n")
  }
  invisible(x)
}

# -- Tool factories ------------------------------------------------------------

#' Create a tool returning available filters metadata
#'
#' Returns a \code{\link{cb_tool}} whose function takes no arguments and returns
#' a JSON string with filter metadata from \code{\link{shape}}.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_filters_meta <- function(cohort) {
  fun <- function() {
    source <- cohort$get_source()
    if (is.null(source$available_filters) || length(source$available_filters) == 0L) {
      return("No filters metadata available. Use autofilter(attach_as = 'meta') on the source first.")
    }
    filters_meta <- shape(source)
    as.character(jsonlite::toJSON(filters_meta, auto_unbox = TRUE))
  }

  cb_tool(
    fun = fun,
    name = "cb_get_filters_meta",
    description = paste(
      "Returns information about available filters in JSON format.",
      "The JSON is a set of objects, each describing either a dataset",
      "(when the 'filter' field is NA) or a filter (otherwise).",
      "The 'filter' field stores the filter id.",
      "The 'dataset' field stores the dataset name the filter belongs to.",
      "The 'description' field stores the filter's purpose.",
      "The 'stats' field stores filter limits:",
      "'choices' lists available options,",
      "'range' provides the numerical bounds the filter operates within."
    )
  )
}

#' Create a tool for adding filters to a cohort
#'
#' Returns a \code{\link{cb_tool}} that adds selected filters from the source's
#' \code{available_filters} to the cohort. The \code{action} parameter controls
#' whether filters are added to a new step or appended to the last existing step.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @param action One of \code{"new_step"} (default) or \code{"edit_last"}.
#'   This is set by the developer, not exposed to the LLM.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_add_filters <- function(cohort, action = c("new_step", "edit_last")) {
  rlang::check_installed("ellmer", reason = "to create cohort AI tools")
  action <- match.arg(action)

  fun <- function(filter_ids) {
    filter_ids <- trimws(strsplit(filter_ids, ",")[[1L]])
    available <- cohort$get_source()$available_filters

    if (is.null(available) || length(available) == 0L) {
      return("No available filters. Use autofilter(attach_as = 'meta') on the source first.")
    }

    matching <- purrr::keep(available, function(f) f@id %in% filter_ids)
    if (length(matching) == 0L) {
      available_ids <- purrr::map_chr(available, ~ .x@id)
      return(glue::glue(
        "No filters found matching: {paste(filter_ids, collapse = ', ')}. ",
        "Available filter ids: {paste(available_ids, collapse = ', ')}"
      ))
    }

    matched_ids <- purrr::map_chr(matching, ~ .x@id)
    unknown <- setdiff(filter_ids, matched_ids)

    if (action == "new_step") {
      cohort$copy_step(filters = matching, run_flow = FALSE)
    } else {
      if (cohort$last_step_id() == "0") {
        cohort$add_step(step())
      }
      step_id <- cohort$last_step_id()
      for (f in matching) {
        state <- get_filter_state(f, extra_fields = NULL)
        cohort$add_filter(do.call(filter, state), step_id = step_id)
      }
    }

    msg <- glue::glue("Filters added ({action}): {paste(matched_ids, collapse = ', ')}")
    if (length(unknown) > 0L) {
      msg <- glue::glue("{msg}. Unknown filter ids ignored: {paste(unknown, collapse = ', ')}")
    }
    as.character(msg)
  }

  cb_tool(
    fun = fun,
    name = "cb_add_filters",
    description = paste(
      "Adds a set of filters to the cohort.",
      "Available filter ids can be found using the 'cb_get_filters_meta' tool.",
      "Important: call this tool once with all desired filter ids."
    ),
    arguments = list(
      filter_ids = ellmer::type_string(
        "Comma-separated filter ids to add to the cohort."
      )
    )
  )
}

#' Create a tool for setting filter values
#'
#' Returns a \code{\link{cb_tool}} that updates filter parameter values on the
#' last step of the cohort and triggers the data pipeline.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_set_filter_values <- function(cohort) {
  rlang::check_installed("ellmer", reason = "to create cohort AI tools")

  fun <- function(filter_values) {
    filter_vals <- tryCatch(
      jsonlite::fromJSON(filter_values),
      error = function(e) NULL
    )
    if (is.null(filter_vals)) {
      return("Invalid JSON input. Please provide a valid JSON object.")
    }

    step_id <- cohort$last_step_id()
    if (step_id == "0") {
      return("No steps in the cohort. Add filters first using 'cb_add_filters'.")
    }

    updated <- character(0L)
    for (filter_id in names(filter_vals)) {
      tryCatch(
        {
          do.call(
            cohort$update_filter,
            c(
              list(step_id = step_id, filter_id = filter_id),
              filter_vals[[filter_id]]
            )
          )
          updated <- c(updated, filter_id)
        },
        error = function(e) {
          warning(glue::glue("Failed to update filter '{filter_id}': {conditionMessage(e)}"))
        }
      )
    }

    run(cohort)

    if (length(updated) == 0L) {
      return("No filters were updated.")
    }
    as.character(glue::glue("Updated filters: {paste(updated, collapse = ', ')}"))
  }

  cb_tool(
    fun = fun,
    name = "cb_set_filter_values",
    description = paste(
      "Sets filter values on the cohort's last step and runs the data pipeline.",
      "Filter domains can be found using the 'cb_get_filters_meta' tool (stats field)."
    ),
    arguments = list(
      filter_values = ellmer::type_string(paste(
        "JSON object with filter values.",
        "Each key is a filter id. The value is an object with:",
        "'value' - array of values for discrete-type filters,",
        "'range' - array of two numbers [min, max] for range-type filters."
      ))
    )
  )
}

# -- Registration functions ----------------------------------------------------

#' Register cohortBuilder tools with an ellmer chat
#'
#' \code{cb_register_tool} registers a single \code{\link{cb_tool}} with
#' an \pkg{ellmer} chat object. \code{cb_register_tools} is a convenience
#' wrapper that registers all three built-in tools at once.
#'
#' @param chat An \pkg{ellmer} chat object (e.g. from \code{ellmer::chat_openai()}).
#' @param tool A \code{\link{cb_tool}} object.
#' @return The \code{chat} object, invisibly (for piping).
#'
#' @examples
#' \dontrun{
#' source <- set_source(tblist(iris = iris)) |> autofilter(attach_as = "meta")
#' coh <- cohort(source)
#' chat <- ellmer::chat_openai()
#' chat |> cb_register_tools(coh)
#' chat$chat("Show me the available filters")
#' }
#'
#' @export
cb_register_tool <- function(chat, tool) {
  rlang::check_installed("ellmer", reason = "to register cohort tools")
  if (!inherits(tool, "cb_tool")) {
    stop("`tool` must be a 'cb_tool' object.", call. = FALSE)
  }

  tool_def <- ellmer::tool(
    tool$fun,
    name = tool$name,
    description = tool$description,
    arguments = tool$arguments
  )
  chat$register_tool(tool_def)
  invisible(chat)
}

#' @rdname cb_register_tool
#' @param cohort A \code{\link{Cohort}} object.
#' @param action Passed to \code{\link{cb_tool_add_filters}}.
#' @export
cb_register_tools <- function(chat, cohort, action = c("new_step", "edit_last")) {
  action <- match.arg(action)
  chat |>
    cb_register_tool(cb_tool_filters_meta(cohort)) |>
    cb_register_tool(cb_tool_add_filters(cohort, action = action)) |>
    cb_register_tool(cb_tool_set_filter_values(cohort))
}
