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
    print("cb_tool_filters_meta")

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
#' \code{available_filters} to the cohort. The LLM chooses whether to add to a
#' new step or the existing last step via the \code{action} argument.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_add_filters <- function(cohort) {
  rlang::check_installed("ellmer", reason = "to create cohort AI tools")

  fun <- function(filter_ids, action = "new_step") {
    print("cb_tool_add_filters")
    print(filter_ids)
    print(action)

    action <- match.arg(action, c("new_step", "edit_last"))
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

    # Guard against duplicate calls (e.g. LLM parallel tool invocations)
    last_id <- cohort$last_step_id()
    if (last_id != "0") {
      existing_ids <- names(cohort$get_step(last_id)$filters)
      if (all(matched_ids %in% existing_ids)) {
        return(as.character(glue::glue(
          "Filters already present in step {last_id}: {paste(matched_ids, collapse = ', ')}"
        )))
      }
    }

    if (action == "new_step") {
      cohort$copy_step(filters = matching, run_flow = FALSE)
    } else {
      if (cohort$last_step_id() == "0") {
        cohort$add_step(step())
      }
      step_id <- cohort$last_step_id()
      for (f in matching) {
        state <- get_filter_params(f)
        cohort$add_filter(do.call(filter, state), step_id = step_id)
      }
    }

    if (getOption("cb_tool_run_cohort", TRUE)) {
      run(cohort)
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
      "Adds a set of filters to the cohort without setting their values.",
      "Use this tool when the user wants to add filters but does not specify concrete values.",
      "If the user provides specific filter values, use 'cb_apply_filters' instead.",
      "Available filter ids can be found using the 'cb_get_filters_meta' tool.",
      "Important: call this tool once with all desired filter ids.",
      "Use action='edit_last' when the user asks to add filters to the current/existing step.",
      "Use action='new_step' (default) when the user wants a new filtering step."
    ),
    arguments = list(
      filter_ids = ellmer::type_string(
        "Comma-separated filter ids to add to the cohort."
      ),
      action = ellmer::type_enum(
        "Whether to create a new step or add to the existing last step.",
        values = c("new_step", "edit_last")
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
    print("cb_tool_set_filter_values")
    print(filter_values)

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

    if (getOption("cb_tool_run_cohort", TRUE)) {
      run(cohort)
    }

    if (length(updated) == 0L) {
      return("No filters were updated.")
    }
    as.character(glue::glue("Updated filters: {paste(updated, collapse = ', ')}"))
  }

  cb_tool(
    fun = fun,
    name = "cb_set_filter_values",
    description = paste(
      "Sets values on filters that already exist in the cohort's last step.",
      "Use this tool when the user wants to update values of previously added filters,",
      "not to add new filters. To add new filters with values, use 'cb_apply_filters'.",
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

#' Create a tool that adds filters and sets their values in one call
#'
#' Returns a \code{\link{cb_tool}} that combines filter addition and value
#' assignment into a single tool call. This avoids issues with LLMs splitting
#' the work across multiple parallel calls.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_apply_filters <- function(cohort) {
  rlang::check_installed("ellmer", reason = "to create cohort AI tools")

  fun <- function(filters, action = "new_step") {
    print("cb_tool_apply_filters")
    print(action)

    action <- match.arg(action, c("new_step", "edit_last"))
    filter_vals <- tryCatch(
      jsonlite::fromJSON(filters),
      error = function(e) NULL
    )
    if (is.null(filter_vals) || length(filter_vals) == 0L) {
      return("Invalid or empty JSON input. Provide a JSON object keyed by filter id.")
    }

    filter_ids <- names(filter_vals)
    available <- cohort$get_source()$available_filters
    if (is.null(available) || length(available) == 0L) {
      return("No available filters. Use autofilter(attach_as = 'meta') on the source first.")
    }

    matching <- purrr::keep(available, function(f) f@id %in% filter_ids)
    if (length(matching) == 0L) {
      available_ids <- purrr::map_chr(available, ~ .x@id)
      return(as.character(glue::glue(
        "No filters found matching: {paste(filter_ids, collapse = ', ')}. ",
        "Available filter ids: {paste(available_ids, collapse = ', ')}"
      )))
    }

    matched_ids <- purrr::map_chr(matching, ~ .x@id)
    unknown <- setdiff(filter_ids, matched_ids)

    # Apply desired values directly to filter objects before adding
    updated <- character(0L)
    for (i in seq_along(matching)) {
      fid <- matched_ids[[i]]
      vals <- filter_vals[[fid]]
      if (is.null(vals) || length(vals) == 0L) next
      state <- get_filter_params(matching[[i]])
      state[names(vals)] <- vals
      matching[[i]] <- tryCatch(
        do.call(filter, state),
        error = function(e) {
          warning(glue::glue("Failed to set values for filter '{fid}': {conditionMessage(e)}"))
          matching[[i]]
        }
      )
      updated <- c(updated, fid)
    }

    # Add filters to the cohort
    if (action == "new_step") {
      print("new step")
      cohort$add_step(do.call(step, matching))
    } else {
      print("edit last step")
      if (cohort$last_step_id() == "0") {
        cohort$add_step(do.call(step, matching))
      } else {
        step_id <- cohort$last_step_id()
        for (f in matching) {
          state <- get_filter_params(f)
          cohort$add_filter(do.call(filter, state), step_id = step_id)
        }
      }
    }

    if (getOption("cb_tool_run_cohort", TRUE)) {
      run(cohort)
    }

    msg <- glue::glue("Filters applied ({action}): {paste(matched_ids, collapse = ', ')}")
    if (length(updated) > 0L) {
      msg <- glue::glue("{msg}. Values set for: {paste(updated, collapse = ', ')}")
    }
    if (length(unknown) > 0L) {
      msg <- glue::glue("{msg}. Unknown filter ids ignored: {paste(unknown, collapse = ', ')}")
    }
    as.character(msg)
  }

  cb_tool(
    fun = fun,
    name = "cb_apply_filters",
    description = paste(
      "Adds filters to the cohort and sets their values in a single operation.",
      "Available filter ids and their domains can be found using 'cb_get_filters_meta' (stats field).",
      "Always use this tool to apply filters - do not call it multiple times for separate filters,",
      "instead include all filters in a single call.",
      "Use action='edit_last' when the user asks to add filters to the current/existing step.",
      "Use action='new_step' (default) when the user wants a new filtering step."
    ),
    arguments = list(
      filters = ellmer::type_string(paste(
        "JSON object keyed by filter id.",
        "Each value is an object with:",
        "'value' - array of values for discrete-type filters,",
        "'range' - array of two numbers [min, max] for range-type filters.",
        "Example: {\"Species\":{\"value\":[\"setosa\"]},\"hp\":{\"range\":[100,335]}}"
      )),
      action = ellmer::type_enum(
        "Whether to create a new step or add to the existing last step.",
        values = c("new_step", "edit_last")
      )
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
#' @export
cb_register_tools <- function(chat, cohort) {
  chat |>
    cb_register_tool(cb_tool_filters_meta(cohort)) |>
    cb_register_tool(cb_tool_add_filters(cohort)) |>
    cb_register_tool(cb_tool_set_filter_values(cohort)) |>
    cb_register_tool(cb_tool_apply_filters(cohort))
}
