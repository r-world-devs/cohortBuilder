# -- Tool logging ---------------------------------------------------------------

#' Log an AI tool invocation
#'
#' Emits an informative message describing which tool was invoked and, when
#' supplied, the arguments it received. Output is only produced when the
#' \code{cb_tool_verbose} option is \code{TRUE} (default \code{FALSE}), so the
#' tools stay silent during normal use and in tests. Messages are sent via
#' \code{\link[base]{message}()} so they route to stderr and can be captured or
#' suppressed independently of the tool's return value.
#'
#' @param tool Name of the invoked tool (its registered LLM-facing name).
#' @param ... Optional named values to report (e.g. \code{filter_ids = ...}).
#' @return Invisibly \code{NULL}.
#' @noRd
cb_tool_log <- function(tool, ...) {
  if (!isTRUE(getOption("cb_tool_verbose", FALSE))) {
    return(invisible(NULL))
  }
  args <- list(...)
  detail <- ""
  if (length(args) > 0L) {
    formatted <- args |>
      purrr::imap_chr(function(value, name) {
        rendered <- if (is.null(value) || length(value) == 0L) {
          "<none>"
        } else {
          toString(format(value))
        }
        paste0(name, " = ", rendered)
      })
    detail <- paste0(" (", paste(formatted, collapse = "; "), ")")
  }
  message("[cohortBuilder AI tool] ", tool, detail)
  invisible(NULL)
}

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
    cat("Arguments:", toString(names(x$arguments)), "\n")
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
    cb_tool_log("cb_get_filters_meta")

    source <- cohort$get_source()
    if (is.null(source$available_filters) || length(source$available_filters) == 0L) {
      return("No filters metadata available. Use autofilter(attach_as = 'meta') on the source first.")
    }
    filters_meta <- shape(source)
    as.character(jsonlite::toJSON(filters_meta, auto_unbox = TRUE, na = "null", null = "null"))
  }

  cb_tool(
    fun = fun,
    name = "cb_get_filters_meta",
    description = paste(
      "Returns metadata about the datasets and available filters in JSON format.",
      "The JSON has two top-level keys:",
      "'datasets' - an object keyed by dataset name, mapping each dataset to its description (or null).",
      "'filters' - an object keyed by filter id, where each value describes one filter with fields:",
      "'name' (the filter's display name),",
      "'dataset' (the dataset the filter belongs to),",
      "'type' (the filter type, e.g. 'discrete', 'range', 'date_range'),",
      "'description' (a human-readable summary combining the filter's purpose and its variables, or null),",
      "'variables' (an array of objects, each with 'name' and 'description', for the columns the filter covers),",
      "'domain' (the set of valid values the filter accepts:",
      "an array of allowed values for discrete-type filters,",
      "or a two-element [min, max] array for range-type filters).",
      "Use the filter id keys when referring to filters in other tools."
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

  fun <- function(filter_ids, action = "new_step", active = NULL) {
    cb_tool_log("cb_add_filters", filter_ids = filter_ids, action = action, active = active)

    action <- match.arg(action, c("new_step", "edit_last"))
    # active = NULL: inherit the available filter's active state (do not touch).
    set_active <- !is.null(active) && !is.na(active) && nzchar(as.character(active))
    active <- if (set_active) !identical(tolower(as.character(active)), "false") else NA
    filter_ids <- trimws(strsplit(filter_ids, ",", fixed = TRUE)[[1L]])
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

    # Override the active state only when explicitly requested.
    if (set_active) {
      matching <- purrr::map(matching, function(f) {
        f@active <- active
        f
      })
    }

    matched_ids <- purrr::map_chr(matching, ~ .x@id)
    unknown <- setdiff(filter_ids, matched_ids)

    skipped_ids <- character(0L)
    if (action == "new_step") {
      cohort$copy_step(filters = matching, run_flow = FALSE)
      added_ids <- matched_ids
    } else {
      if (cohort$last_step_id() == "0") {
        cohort$add_step(step())
      }
      step_id <- cohort$last_step_id()
      existing_ids <- names(cohort$get_step(step_id)$filters)
      # Skip filters already present in the target step; only add the new ones.
      skipped_ids <- intersect(matched_ids, existing_ids)
      to_add <- purrr::keep(matching, function(f) !f@id %in% existing_ids)
      for (f in to_add) {
        state <- get_filter_params(f)
        cohort$add_filter(do.call(filter, state), step_id = step_id)
      }
      added_ids <- purrr::map_chr(to_add, ~ .x@id)
    }

    if (getOption("cb_tool_run_cohort", TRUE) && length(added_ids) > 0L) {
      run(cohort)
    }

    added_label <- if (set_active) glue::glue(", {if (active) 'active' else 'inactive'}") else ""
    msg_parts <- character(0L)
    if (length(added_ids) > 0L) {
      msg_parts <- c(msg_parts, glue::glue(
        "Filters added ({action}{added_label}): {paste(added_ids, collapse = ', ')}"
      ))
    }
    if (length(skipped_ids) > 0L) {
      msg_parts <- c(msg_parts, glue::glue(
        "Already present in step (skipped): {paste(skipped_ids, collapse = ', ')}. ",
        "Use 'cb_set_filter_values' to change their values."
      ))
    }
    if (length(unknown) > 0L) {
      msg_parts <- c(msg_parts, glue::glue("Unknown filter ids ignored: {paste(unknown, collapse = ', ')}"))
    }
    if (length(msg_parts) == 0L) {
      msg_parts <- "No filters added."
    }
    paste(as.character(msg_parts), collapse = " ")
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
      "Use action='new_step' (default) when the user wants a new filtering step.",
      "With action='edit_last', filters that already exist in the step are skipped",
      "(use 'cb_set_filter_values' to change their values).",
      "Only set 'active' when the user explicitly asks for the filters to be",
      "active or inactive; otherwise omit it to keep each filter's default state.",
      "Set active='false' to add the filters in a deactivated state",
      "(e.g. when the user wants them present but not yet applied)."
    ),
    arguments = list(
      filter_ids = ellmer::type_string(
        "Comma-separated filter ids to add to the cohort."
      ),
      action = ellmer::type_enum(
        "Whether to create a new step or add to the existing last step.",
        values = c("new_step", "edit_last")
      ),
      active = ellmer::type_enum(
        paste(
          "Optional. Only provide when the user explicitly asks for the filters to be",
          "active ('true') or inactive ('false'). Omit to keep each filter's default state."
        ),
        values = c("true", "false"),
        required = FALSE
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
    cb_tool_log("cb_set_filter_values", filter_values = filter_values)

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
      "Sets values on filters that ALREADY EXIST in the cohort's last step.",
      "IMPORTANT: Before using this tool, call 'cb_describe_state' to check which",
      "filters are currently in the cohort. Only use this tool for filters listed there.",
      "If the filter does not exist in the step yet, use 'cb_apply_filters' instead",
      "to add it with values in one operation.",
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

  fun <- function(filters, action = "new_step", active = NULL) {
    cb_tool_log("cb_apply_filters", filters = filters, action = action, active = active)

    action <- match.arg(action, c("new_step", "edit_last"))
    # active = NULL: inherit the available filter's active state (do not touch).
    set_active <- !is.null(active) && !is.na(active) && nzchar(as.character(active))
    active <- if (set_active) !identical(tolower(as.character(active)), "false") else NA
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

    # Override the active state of newly added filters only when requested.
    if (set_active) {
      matching <- purrr::map(matching, function(f) {
        f@active <- active
        f
      })
    }

    # Apply desired values directly to filter objects before adding
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
    }

    # Add filters to the cohort
    added_ids <- character(0L)
    updated_ids <- character(0L)
    if (action == "new_step") {
      cohort$add_step(do.call(step, matching))
      added_ids <- matched_ids
    } else if (cohort$last_step_id() == "0") {
      cohort$add_step(do.call(step, matching))
      added_ids <- matched_ids
    } else {
      step_id <- cohort$last_step_id()
      existing_ids <- names(cohort$get_step(step_id)$filters)
      for (i in seq_along(matching)) {
        fid <- matched_ids[[i]]
        if (fid %in% existing_ids) {
          # Filter already in the step: update its values in place rather than
          # re-adding it (which would reset the existing filter).
          vals <- filter_vals[[fid]]
          if (is.null(vals) || length(vals) == 0L) next
          ok <- tryCatch(
            {
              do.call(
                cohort$update_filter,
                c(list(step_id = step_id, filter_id = fid), vals)
              )
              TRUE
            },
            error = function(e) {
              warning(glue::glue("Failed to update filter '{fid}': {conditionMessage(e)}"))
              FALSE
            }
          )
          if (isTRUE(ok)) updated_ids <- c(updated_ids, fid)
        } else {
          state <- get_filter_params(matching[[i]])
          cohort$add_filter(do.call(filter, state), step_id = step_id)
          added_ids <- c(added_ids, fid)
        }
      }
    }

    if (getOption("cb_tool_run_cohort", TRUE)) {
      run(cohort)
    }

    added_label <- if (set_active) glue::glue(", {if (active) 'active' else 'inactive'}") else ""
    msg_parts <- character(0L)
    if (length(added_ids) > 0L) {
      msg_parts <- c(msg_parts, glue::glue(
        "Filters applied ({action}{added_label}): {paste(added_ids, collapse = ', ')}"
      ))
    }
    if (length(updated_ids) > 0L) {
      msg_parts <- c(msg_parts, glue::glue(
        "Updated values for existing filters: {paste(updated_ids, collapse = ', ')}"
      ))
    }
    if (length(unknown) > 0L) {
      msg_parts <- c(msg_parts, glue::glue("Unknown filter ids ignored: {paste(unknown, collapse = ', ')}"))
    }
    if (length(msg_parts) == 0L) {
      msg_parts <- "No filters applied."
    }
    paste(as.character(msg_parts), collapse = " ")
  }

  cb_tool(
    fun = fun,
    name = "cb_apply_filters",
    description = paste(
      "Adds NEW filters to the cohort and sets their values in a single operation.",
      "Prefer this tool for filters that are NOT yet in the cohort.",
      "Call 'cb_describe_state' first to check what filters already exist.",
      "With action='edit_last', any filter that already exists in the step has its",
      "values updated in place instead of being re-added; you may also use",
      "'cb_set_filter_values' to change values of existing filters.",
      "Available filter ids and their domains can be found using 'cb_get_filters_meta' (stats field).",
      "Include all new filters in a single call - do not call this tool multiple times.",
      "Use action='edit_last' when the user asks to add filters to the current/existing step.",
      "Use action='new_step' (default) when the user wants a new filtering step.",
      "Only set 'active' when the user explicitly asks for the filters to be",
      "active or inactive; otherwise omit it to keep each filter's default state.",
      "Set active='false' to add the new filters in a deactivated state",
      "(applies to newly added filters; existing filters keep their current state)."
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
      ),
      active = ellmer::type_enum(
        paste(
          "Optional. Only provide when the user explicitly asks for the filters to be",
          "active ('true') or inactive ('false'). Omit to keep each filter's default state."
        ),
        values = c("true", "false"),
        required = FALSE
      )
    )
  )
}

# -- Cohort state tool ---------------------------------------------------------

#' Create a tool returning the current cohort state
#'
#' Returns a \code{\link{cb_tool}} whose function takes no arguments and returns
#' a structured text summary of all steps, filters, their active status, and
#' pending state.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_describe_state <- function(cohort) {
  fun <- function() {
    cb_tool_log("cb_describe_state")

    steps <- cohort$get_step()
    if (length(steps) == 0L) {
      return("No steps configured in the cohort.")
    }

    cohort$describe_state(to_string = TRUE)
  }

  cb_tool(
    fun = fun,
    name = "cb_describe_state",
    description = paste(
      "Returns the current cohort state: all steps and their filters.",
      "Each step shows its ID and whether it is pending (needs recalculation).",
      "Each filter shows its ID, type, dataset, and active/inactive status.",
      "IMPORTANT: Call this tool before modifying the cohort to determine the right action:",
      "- If a filter already exists in a step, use 'cb_set_filter_values' to change its values.",
      "- If a filter is not yet in the cohort, use 'cb_apply_filters' to add it with values.",
      "Also use before toggling, removing, or running the cohort."
    )
  )
}

# -- Toggle filters tool -------------------------------------------------------

#' Create a tool for activating or deactivating filters
#'
#' Returns a \code{\link{cb_tool}} that toggles the active state of existing
#' filters in the cohort.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_toggle_filters <- function(cohort) {
  rlang::check_installed("ellmer", reason = "to create cohort AI tools")

  fun <- function(filter_ids, active, step_id = NULL) {
    cb_tool_log("cb_toggle_filters", filter_ids = filter_ids, active = active, step_id = step_id)

    active <- as.logical(active)
    if (is.na(active)) {
      return("Invalid 'active' value. Must be 'true' or 'false'.")
    }

    filter_ids <- trimws(strsplit(filter_ids, ",", fixed = TRUE)[[1L]])
    steps <- cohort$get_step()
    if (length(steps) == 0L) {
      return("No steps configured in the cohort.")
    }

    if (is.null(step_id) || step_id == "") {
      if (length(steps) > 1L) {
        step_ids <- purrr::map_chr(steps, "id")
        return(as.character(glue::glue(
          "Multiple steps exist: {paste(step_ids, collapse = ', ')}. ",
          "Please specify a step_id."
        )))
      }
      step_id <- cohort$last_step_id()
    }

    step <- cohort$get_step(step_id)
    if (is.null(step)) {
      return(as.character(glue::glue("Step '{step_id}' not found.")))
    }

    existing_ids <- names(step$filters)
    matched <- intersect(filter_ids, existing_ids)
    unknown <- setdiff(filter_ids, existing_ids)

    if (length(matched) == 0L) {
      return(as.character(glue::glue(
        "No matching filters in step {step_id}. ",
        "Available: {paste(existing_ids, collapse = ', ')}"
      )))
    }

    for (fid in matched) {
      cohort$update_filter(step_id = step_id, filter_id = fid, active = active)
    }

    if (getOption("cb_tool_run_cohort", TRUE)) {
      run(cohort)
    }

    action_label <- if (active) "Activated" else "Deactivated"
    msg <- glue::glue("{action_label} filters in step {step_id}: {paste(matched, collapse = ', ')}")
    if (length(unknown) > 0L) {
      msg <- glue::glue("{msg}. Unknown filter ids ignored: {paste(unknown, collapse = ', ')}")
    }
    as.character(msg)
  }

  cb_tool(
    fun = fun,
    name = "cb_toggle_filters",
    description = paste(
      "Activates or deactivates existing filters in the cohort.",
      "Inactive filters are skipped during data filtering.",
      "Use 'cb_describe_state' first to see which filters exist and their current state.",
      "When multiple steps exist, you must specify step_id;",
      "with a single step it defaults to that step."
    ),
    arguments = list(
      filter_ids = ellmer::type_string(
        "Comma-separated filter ids to activate or deactivate."
      ),
      active = ellmer::type_enum(
        "Whether to activate (true) or deactivate (false) the filters.",
        values = c("true", "false")
      ),
      step_id = ellmer::type_string(
        "Step id containing the filters. Required when multiple steps exist, optional otherwise."
      )
    )
  )
}

# -- Run cohort tool -----------------------------------------------------------

#' Create a tool for running the cohort pipeline
#'
#' Returns a \code{\link{cb_tool}} that triggers data calculations for the
#' entire cohort or a specific step. Only functional when the
#' \code{cb_tool_run_cohort} option is \code{FALSE}; otherwise returns an
#' informative message that the cohort runs automatically.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_run <- function(cohort) {
  rlang::check_installed("ellmer", reason = "to create cohort AI tools")

  fun <- function(step_id = NULL) {
    cb_tool_log("cb_run", step_id = step_id)

    if (getOption("cb_tool_run_cohort", TRUE)) {
      return(paste(
        "The cohort is configured to run automatically after each modification",
        "(option 'cb_tool_run_cohort' is TRUE).",
        "Manual run is not needed."
      ))
    }

    if (!is.null(step_id) && step_id != "") {
      steps <- cohort$get_step()
      if (!step_id %in% names(steps)) {
        return(as.character(glue::glue("Step '{step_id}' not found.")))
      }
      run(cohort, step_id = step_id)
      return(as.character(glue::glue("Step {step_id} executed.")))
    }

    run(cohort)
    "Cohort pipeline executed for all steps."
  }

  cb_tool(
    fun = fun,
    name = "cb_run",
    description = paste(
      "Runs the cohort data pipeline.",
      "When auto-run is enabled (default), this tool returns an informative",
      "message that manual execution is not needed.",
      "When auto-run is disabled (option 'cb_tool_run_cohort' is FALSE),",
      "this tool triggers data calculations for all steps or a specific step."
    ),
    arguments = list(
      step_id = ellmer::type_string(
        "Optional step id to run a single step. Omit or leave empty to run all steps."
      )
    )
  )
}

# -- Remove filters tool -------------------------------------------------------

#' Create a tool for removing filters from the cohort
#'
#' Returns a \code{\link{cb_tool}} that removes filters from a step in the
#' cohort. If removing all filters from a step, the entire step is removed.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_remove_filters <- function(cohort) {
  rlang::check_installed("ellmer", reason = "to create cohort AI tools")

  fun <- function(filter_ids, step_id = NULL) {
    cb_tool_log("cb_remove_filters", filter_ids = filter_ids, step_id = step_id)

    filter_ids <- trimws(strsplit(filter_ids, ",", fixed = TRUE)[[1L]])
    steps <- cohort$get_step()
    if (length(steps) == 0L) {
      return("No steps configured in the cohort.")
    }

    if (is.null(step_id) || step_id == "") {
      if (length(steps) > 1L) {
        step_ids <- purrr::map_chr(steps, "id")
        return(as.character(glue::glue(
          "Multiple steps exist: {paste(step_ids, collapse = ', ')}. ",
          "Please specify a step_id."
        )))
      }
      step_id <- cohort$last_step_id()
    }

    step <- cohort$get_step(step_id)
    if (is.null(step)) {
      return(as.character(glue::glue("Step '{step_id}' not found.")))
    }

    existing_ids <- names(step$filters)
    matched <- intersect(filter_ids, existing_ids)
    unknown <- setdiff(filter_ids, existing_ids)

    if (length(matched) == 0L) {
      return(as.character(glue::glue(
        "No matching filters in step {step_id}. ",
        "Available: {paste(existing_ids, collapse = ', ')}"
      )))
    }

    for (fid in matched) {
      cohort$remove_filter(step_id = step_id, filter_id = fid)
    }

    if (getOption("cb_tool_run_cohort", TRUE)) {
      run(cohort)
    }

    msg <- glue::glue("Removed filters from step {step_id}: {paste(matched, collapse = ', ')}")
    if (length(unknown) > 0L) {
      msg <- glue::glue("{msg}. Unknown filter ids ignored: {paste(unknown, collapse = ', ')}")
    }
    as.character(msg)
  }

  cb_tool(
    fun = fun,
    name = "cb_remove_filters",
    description = paste(
      "Removes filters from the cohort.",
      "Use 'cb_describe_state' first to see which filters exist.",
      "When multiple steps exist, you must specify step_id;",
      "with a single step it defaults to that step.",
      "If all filters are removed from a step, the step itself is removed."
    ),
    arguments = list(
      filter_ids = ellmer::type_string(
        "Comma-separated filter ids to remove."
      ),
      step_id = ellmer::type_string(
        "Step id from which to remove the filters. Required when multiple steps exist, optional otherwise."
      )
    )
  )
}

# -- Data summary tool ----------------------------------------------------------

#' Create a tool returning row counts per dataset and step
#'
#' Returns a \code{\link{cb_tool}} that reports how many rows remain in each
#' dataset at each step (before and after filtering). This is the primary tool
#' for understanding the impact of applied filters.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_get_data_summary <- function(cohort) {
  fun <- function() {
    cb_tool_log("cb_get_data_summary")

    steps <- cohort$get_step()
    if (length(steps) == 0L) {
      return("No steps configured. Add and run filters first.")
    }

    pending <- cohort$is_pending()
    if (any(pending)) {
      pending_ids <- names(pending[pending])
      return(as.character(glue::glue(
        "Steps {paste(pending_ids, collapse = ', ')} are pending. ",
        "Run the cohort first to get data summaries."
      )))
    }

    source <- cohort$get_source()
    dataset_names <- names(source$dtconn)

    lines <- character(0L)

    # Initial (step 0) row counts
    pre_data <- cohort$get_data(step_id = 1L, state = "pre")
    counts <- purrr::map_int(dataset_names, ~ nrow(pre_data[[.x]]))
    lines <- c(lines, "Initial data:")
    for (i in seq_along(dataset_names)) {
      lines <- c(lines, glue::glue("  {dataset_names[i]}: {counts[i]} rows"))
    }

    # Per-step row counts
    for (step in steps) {
      post_data <- cohort$get_data(step_id = step$id, state = "post")
      counts <- purrr::map_int(dataset_names, ~ nrow(post_data[[.x]]))
      lines <- c(lines, glue::glue("After step {step$id}:"))
      for (i in seq_along(dataset_names)) {
        lines <- c(lines, glue::glue("  {dataset_names[i]}: {counts[i]} rows"))
      }
    }

    paste(lines, collapse = "\n")
  }

  cb_tool(
    fun = fun,
    name = "cb_get_data_summary",
    description = paste(
      "Returns row counts for each dataset at each filtering step.",
      "Shows initial counts and counts after each step, so you can see",
      "how many rows were filtered out. The cohort must be run first;",
      "if steps are pending, this tool will ask you to run it."
    )
  )
}

# -- Clear filters tool --------------------------------------------------------

#' Create a tool for resetting filters to defaults
#'
#' Returns a \code{\link{cb_tool}} that resets filter values to their defaults
#' without removing the filters from the cohort.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_clear_filters <- function(cohort) {
  rlang::check_installed("ellmer", reason = "to create cohort AI tools")

  fun <- function(filter_ids = NULL, step_id = NULL) {
    cb_tool_log("cb_clear_filters", filter_ids = filter_ids, step_id = step_id)

    steps <- cohort$get_step()
    if (length(steps) == 0L) {
      return("No steps configured in the cohort.")
    }

    if (is.null(step_id) || step_id == "") {
      if (length(steps) > 1L) {
        step_ids <- purrr::map_chr(steps, "id")
        return(as.character(glue::glue(
          "Multiple steps exist: {paste(step_ids, collapse = ', ')}. ",
          "Please specify a step_id."
        )))
      }
      step_id <- cohort$last_step_id()
    }

    step <- cohort$get_step(step_id)
    if (is.null(step)) {
      return(as.character(glue::glue("Step '{step_id}' not found.")))
    }

    # Clear all filters in step or specific ones
    if (is.null(filter_ids) || filter_ids == "") {
      cohort$clear_step(step_id)
      if (getOption("cb_tool_run_cohort", TRUE)) {
        run(cohort)
      }
      return(as.character(glue::glue(
        "All filters in step {step_id} reset to defaults."
      )))
    }

    filter_ids <- trimws(strsplit(filter_ids, ",", fixed = TRUE)[[1L]])
    existing_ids <- names(step$filters)
    matched <- intersect(filter_ids, existing_ids)
    unknown <- setdiff(filter_ids, existing_ids)

    if (length(matched) == 0L) {
      return(as.character(glue::glue(
        "No matching filters in step {step_id}. ",
        "Available: {paste(existing_ids, collapse = ', ')}"
      )))
    }

    for (fid in matched) {
      cohort$clear_filter(step_id, fid)
    }

    if (getOption("cb_tool_run_cohort", TRUE)) {
      run(cohort)
    }

    msg <- glue::glue("Reset filters in step {step_id}: {paste(matched, collapse = ', ')}")
    if (length(unknown) > 0L) {
      msg <- glue::glue("{msg}. Unknown filter ids ignored: {paste(unknown, collapse = ', ')}")
    }
    as.character(msg)
  }

  cb_tool(
    fun = fun,
    name = "cb_clear_filters",
    description = paste(
      "Resets filters to their default values without removing them.",
      "This undoes any value changes while keeping the filters in place.",
      "Omit filter_ids to reset all filters in the step.",
      "Use 'cb_describe_state' first to see which filters exist.",
      "When multiple steps exist, you must specify step_id;",
      "with a single step it defaults to that step."
    ),
    arguments = list(
      filter_ids = ellmer::type_string(
        "Comma-separated filter ids to reset. Omit or leave empty to reset all filters in the step."
      ),
      step_id = ellmer::type_string(
        "Step id containing the filters. Required when multiple steps exist, optional otherwise."
      )
    )
  )
}

# -- Get code tool -------------------------------------------------------------

#' Create a tool returning reproducible filtering code
#'
#' Returns a \code{\link{cb_tool}} that generates reproducible R code for the
#' current cohort filtering pipeline via \code{get_code()}.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_get_code <- function(cohort) {
  fun <- function() {
    cb_tool_log("cb_get_code")

    steps <- cohort$get_step()
    if (length(steps) == 0L) {
      return("No steps configured. Add filters first.")
    }

    result <- tryCatch(
      utils::capture.output(code(cohort)),
      error = function(e) NULL
    )

    if (is.null(result) || length(result) == 0L) {
      return("Unable to generate code. Make sure the cohort has been run.")
    }

    paste(result, collapse = "\n")
  }

  cb_tool(
    fun = fun,
    name = "cb_get_code",
    description = paste(
      "Returns reproducible R code for the current filtering pipeline.",
      "The code recreates the source, applies all active filters, and can be",
      "run independently to reproduce the filtered data.",
      "Use this when the user asks to export, share, or see the filtering code."
    )
  )
}

# -- Remove step tool ----------------------------------------------------------

#' Create a tool for removing the last step
#'
#' Returns a \code{\link{cb_tool}} that removes the last step from the cohort,
#' including all its filters.
#'
#' @param cohort A \code{\link{Cohort}} object.
#' @return A \code{cb_tool} object.
#'
#' @export
cb_tool_remove_step <- function(cohort) {
  fun <- function() {
    cb_tool_log("cb_remove_step")

    steps <- cohort$get_step()
    if (length(steps) == 0L) {
      return("No steps to remove.")
    }

    step_id <- cohort$last_step_id()
    filter_ids <- names(steps[[step_id]]$filters)
    cohort$remove_step(step_id)

    if (getOption("cb_tool_run_cohort", TRUE) && length(cohort$get_step()) > 0L) {
      run(cohort)
    }

    as.character(glue::glue(
      "Removed step {step_id} (contained filters: {paste(filter_ids, collapse = ', ')}). ",
      "Remaining steps: {length(cohort$get_step())}"
    ))
  }

  cb_tool(
    fun = fun,
    name = "cb_remove_step",
    description = paste(
      "Removes the last step from the cohort, including all its filters.",
      "Use 'cb_describe_state' to see the current steps before removing.",
      "To remove individual filters without removing the step, use 'cb_remove_filters'."
    )
  )
}

# -- Registration functions ----------------------------------------------------

#' Register cohortBuilder tools with an ellmer chat
#'
#' \code{cb_register_tool} registers a single \code{\link{cb_tool}} with
#' an \pkg{ellmer} chat object. \code{cb_register_tools} is a convenience
#' wrapper that registers all built-in tools at once.
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
    cb_register_tool(cb_tool_describe_state(cohort)) |>
    cb_register_tool(cb_tool_get_data_summary(cohort)) |>
    cb_register_tool(cb_tool_get_code(cohort)) |>
    cb_register_tool(cb_tool_add_filters(cohort)) |>
    cb_register_tool(cb_tool_set_filter_values(cohort)) |>
    cb_register_tool(cb_tool_apply_filters(cohort)) |>
    cb_register_tool(cb_tool_toggle_filters(cohort)) |>
    cb_register_tool(cb_tool_clear_filters(cohort)) |>
    cb_register_tool(cb_tool_remove_filters(cohort)) |>
    cb_register_tool(cb_tool_remove_step(cohort)) |>
    cb_register_tool(cb_tool_run(cohort))
}
