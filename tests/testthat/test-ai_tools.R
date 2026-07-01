# -- Test setup -----------------------------------------------------------------

make_test_cohort <- function() {
  source <- set_source(
    tblist(iris = iris, mtcars = mtcars),
    description = list(
      iris = list(
        dataset_ = describe("iris plants data"),
        Species = describe("species of iris")
      ),
      mtcars = list(
        dataset_ = describe("car specifications"),
        hp = describe("gross horsepower")
      )
    )
  ) |> autofilter(attach_as = "meta")
  cohort(source = source)
}

MockChat <- R6::R6Class("MockChat", public = list(
  tools = list(),
  register_tool = function(tool_def) {
    self$tools[[length(self$tools) + 1L]] <- tool_def
    invisible(self)
  }
))

# -- cb_tool class -------------------------------------------------------------

test_that("cb_tool creates object with correct class and structure", {
  t <- cb_tool(
    fun = identity,
    name = "test_tool",
    description = "A test tool"
  )
  expect_s3_class(t, "cb_tool")
  expect_identical(t$name, "test_tool")
  expect_identical(t$description, "A test tool")
  expect_type(t$fun, "closure")
  expect_identical(t$arguments, list())
})

test_that("cb_tool with arguments preserves them", {
  t <- cb_tool(
    fun = identity,
    name = "test",
    description = "test",
    arguments = list(x = "type_placeholder")
  )
  expect_named(t$arguments, "x")
})

test_that("print.cb_tool produces expected output", {
  t <- cb_tool(
    fun = identity,
    name = "my_tool",
    description = "Does something useful",
    arguments = list(x = "a", y = "b")
  )
  out <- capture.output(print(t))
  expect_true(any(grepl("my_tool", out, fixed = TRUE)))
  expect_true(any(grepl("Does something useful", out, fixed = TRUE)))
  expect_true(any(grepl("x, y", out, fixed = TRUE)))
})

# -- cb_tool_filters_meta ------------------------------------------------------

test_that("cb_tool_filters_meta returns cb_tool", {
  coh <- make_test_cohort()
  t <- cb_tool_filters_meta(coh)
  expect_s3_class(t, "cb_tool")
  expect_identical(t$name, "cb_get_filters_meta")
  expect_identical(t$arguments, list())
})

test_that("cb_tool_filters_meta fun returns valid JSON", {
  coh <- make_test_cohort()
  t <- cb_tool_filters_meta(coh)
  result <- t$fun()
  expect_type(result, "character")
  parsed <- jsonlite::fromJSON(result, simplifyVector = FALSE)
  expect_named(parsed, c("datasets", "filters"))
  expect_true("iris" %in% names(parsed$datasets))
  expect_true("iris-Species" %in% names(parsed$filters))
  species <- parsed$filters[["iris-Species"]]
  expect_identical(species$dataset, "iris")
  expect_identical(species$type, "discrete")
  expect_true(nzchar(species$description))
  expect_identical(species$variables[[1L]]$name, "Species")
})

test_that("cb_tool_filters_meta handles missing available_filters", {
  source <- set_source(tblist(iris = iris))
  coh <- cohort(source = source)
  t <- cb_tool_filters_meta(coh)
  result <- t$fun()
  expect_true(grepl("No filters metadata", result, fixed = TRUE))
})

# -- cb_tool_add_filters -------------------------------------------------------

test_that("cb_tool_add_filters returns cb_tool", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)
  expect_s3_class(t, "cb_tool")
  expect_identical(t$name, "cb_add_filters")

  expect_true(all(c("filter_ids", "action") %in% names(t$arguments)))
})

test_that("cb_tool_add_filters with new_step creates a step", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)

  result <- t$fun("iris-Species, mtcars-hp", action = "new_step")
  expect_true(grepl("iris-Species", result, fixed = TRUE))
  expect_true(grepl("mtcars-hp", result, fixed = TRUE))
  expect_identical(coh$last_step_id(), "1")
})

test_that("cb_tool_add_filters with edit_last adds to existing step", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  # Create initial step
  coh$add_step(step(
    filter("discrete", id = "Species", dataset = "iris", variable = "Species")
  ))
  expect_identical(coh$last_step_id(), "1")

  t <- cb_tool_add_filters(coh)
  result <- t$fun("mtcars-hp", action = "edit_last")
  expect_true(grepl("mtcars-hp", result, fixed = TRUE))
  # Still step 1, filter was added to it
  expect_identical(coh$last_step_id(), "1")
  filters <- coh$get_step("1")$filters
  expect_true("mtcars-hp" %in% names(filters))
})

test_that("cb_tool_add_filters skips filters already present in the step", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)

  t$fun("iris-Species", action = "new_step")
  result <- t$fun("iris-Species", action = "edit_last")

  expect_true(grepl("Already present in step (skipped)", result, fixed = TRUE))
  expect_true(grepl("iris-Species", result, fixed = TRUE))
  # No duplicate added: still a single Species filter.
  expect_identical(sum(names(coh$get_step("1")$filters) == "iris-Species"), 1L)
})

test_that("cb_tool_apply_filters updates values of existing filters in place", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_apply_filters(coh)

  t$fun('{"iris-Species":{"value":["setosa"]}}', action = "new_step")
  result <- t$fun('{"iris-Species":{"value":["versicolor"]}}', action = "edit_last")

  expect_true(grepl("Updated values for existing filters", result, fixed = TRUE))
  # Value updated in place, filter not reset or duplicated.
  filters <- coh$get_step(coh$last_step_id())$filters
  expect_identical(filters[["iris-Species"]]@value, "versicolor")
  expect_identical(sum(names(filters) == "iris-Species"), 1L)
})

test_that("cb_tool_add_filters can add filters as inactive", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)

  result <- t$fun("iris-Species", action = "new_step", active = "false")
  expect_true(grepl("inactive", result, fixed = TRUE))
  expect_false(coh$get_step("1")$filters[["iris-Species"]]@active)
})

test_that("cb_tool_add_filters inherits active state when active is omitted", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  # Force the available Species filter to be inactive by default.
  src <- coh$get_source()
  src$available_filters <- purrr::map(src$available_filters, function(f) {
    if (f@id == "iris-Species") f@active <- FALSE
    f
  })
  t <- cb_tool_add_filters(coh)

  result <- t$fun("iris-Species", action = "new_step")
  expect_false(grepl("active|inactive", result, fixed = TRUE))
  expect_false(coh$get_step("1")$filters[["iris-Species"]]@active)
})

test_that("cb_tool_apply_filters can add filters as inactive", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_apply_filters(coh)

  result <- t$fun('{"iris-Species":{"value":["setosa"]}}', action = "new_step", active = "false")
  expect_true(grepl("inactive", result, fixed = TRUE))
  flt <- coh$get_step(coh$last_step_id())$filters[["iris-Species"]]
  expect_false(flt@active)
  expect_identical(flt@value, "setosa")
})

test_that("cb_tool_add_filters reports unknown filter ids", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)
  result <- t$fun("iris-Species, nonexistent_filter")
  expect_true(grepl("Unknown filter ids ignored", result, fixed = TRUE))
  expect_true(grepl("nonexistent_filter", result, fixed = TRUE))
})

test_that("cb_tool_add_filters handles all unknown ids", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)
  result <- t$fun("fake_one, fake_two")
  expect_true(grepl("No filters found matching", result, fixed = TRUE))
})

test_that("cb_tool_add_filters handles no available_filters", {
  skip_if_not_installed("ellmer")
  source <- set_source(tblist(iris = iris))
  coh <- cohort(source = source)
  t <- cb_tool_add_filters(coh)
  result <- t$fun("Species")
  expect_true(grepl("No available filters", result, fixed = TRUE))
})

# -- cb_tool_set_filter_values -------------------------------------------------

test_that("cb_tool_set_filter_values returns cb_tool", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_set_filter_values(coh)
  expect_s3_class(t, "cb_tool")
  expect_identical(t$name, "cb_set_filter_values")
  expect_named(t$arguments, "filter_values")
})

test_that("cb_tool_set_filter_values updates filters and runs flow", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  coh |> add_filter(
    filter("discrete", id = "Species", dataset = "iris", variable = "Species")
  )
  run(coh)

  t <- cb_tool_set_filter_values(coh)
  result <- t$fun('{"Species": {"value": ["setosa"]}}')
  expect_true(grepl("Updated filters.*Species", result))

  updated_filter <- coh$get_filter("1", "Species")
  expect_identical(updated_filter@value, "setosa")
})

test_that("cb_tool_set_filter_values handles invalid JSON", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  coh |> add_filter(
    filter("discrete", id = "Species", dataset = "iris", variable = "Species")
  )

  t <- cb_tool_set_filter_values(coh)
  result <- t$fun("not valid json{{{")
  expect_true(grepl("Invalid JSON", result, fixed = TRUE))
})

test_that("cb_tool_set_filter_values handles no steps", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_set_filter_values(coh)
  result <- t$fun('{"Species": {"value": ["setosa"]}}')
  expect_true(grepl("No steps", result, fixed = TRUE))
})

# -- Registration functions ----------------------------------------------------

test_that("cb_register_tool registers tool with chat", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  chat <- MockChat$new()
  t <- cb_tool_filters_meta(coh)
  expect_invisible(cb_register_tool(chat, t))
  expect_length(chat$tools, 1L)
})

test_that("cb_register_tool rejects non-cb_tool", {
  skip_if_not_installed("ellmer")
  chat <- MockChat$new()
  expect_error(
    cb_register_tool(chat, list(fun = identity)),
    "cb_tool"
  )
})

test_that("cb_register_tools registers all twelve tools", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  chat <- MockChat$new()
  cb_register_tools(chat, coh)
  expect_length(chat$tools, 12L)
})

# -- cb_tool_describe_state ----------------------------------------------------

test_that("cb_tool_describe_state reports empty and populated cohorts", {
  coh <- make_test_cohort()
  t <- cb_tool_describe_state(coh)
  expect_identical(t$name, "cb_describe_state")
  expect_identical(t$fun(), "No steps configured in the cohort.")

  coh$add_step(step(
    filter("discrete", id = "iris-Species", dataset = "iris", variable = "Species")
  ))
  out <- t$fun()
  expect_true(grepl("Step ID: 1", out, fixed = TRUE))
  expect_true(grepl("iris-Species", out, fixed = TRUE))
})

# -- cb_tool_get_data_summary --------------------------------------------------

test_that("cb_tool_get_data_summary returns row counts per step", {
  coh <- make_test_cohort()
  t <- cb_tool_get_data_summary(coh)

  # No steps yet.
  expect_true(grepl("No steps configured", t$fun(), fixed = TRUE))

  coh$add_step(step(
    filter("discrete", id = "iris-Species", dataset = "iris",
           variable = "Species", value = "setosa")
  ), run_flow = TRUE)

  out <- t$fun()
  expect_true(grepl("Initial data:", out, fixed = TRUE))
  expect_true(grepl("iris: 150 rows", out, fixed = TRUE))
  expect_true(grepl("After step 1:", out, fixed = TRUE))
  expect_true(grepl("iris: 50 rows", out, fixed = TRUE))
})

test_that("cb_tool_get_data_summary asks to run when steps are pending", {
  coh <- make_test_cohort()
  coh$add_step(step(
    filter("discrete", id = "iris-Species", dataset = "iris", variable = "Species")
  ))
  # Step added without running -> pending.
  out <- cb_tool_get_data_summary(coh)$fun()
  expect_true(grepl("pending", out, fixed = TRUE))
})

# -- cb_tool_toggle_filters ----------------------------------------------------

test_that("cb_tool_toggle_filters activates and deactivates filters", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  coh$add_step(step(
    filter("discrete", id = "iris-Species", dataset = "iris", variable = "Species")
  ))
  t <- cb_tool_toggle_filters(coh)

  out <- t$fun("iris-Species", active = "false")
  expect_true(grepl("Deactivated", out, fixed = TRUE))
  expect_false(coh$get_filter("1", "iris-Species")@active)

  out <- t$fun("iris-Species", active = "true")
  expect_true(grepl("Activated", out, fixed = TRUE))
  expect_true(coh$get_filter("1", "iris-Species")@active)
})

test_that("cb_tool_toggle_filters validates inputs and step selection", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_toggle_filters(coh)

  # Invalid active value.
  expect_true(grepl("Invalid 'active'", t$fun("iris-Species", active = "maybe"), fixed = TRUE))
  # No steps.
  expect_true(grepl("No steps configured", t$fun("iris-Species", active = "true"), fixed = TRUE))

  # Multiple steps require an explicit step_id.
  coh$add_step(step(filter("discrete", id = "iris-Species", dataset = "iris", variable = "Species")))
  coh$add_step(step(filter("discrete", id = "mtcars-hp", dataset = "mtcars", variable = "hp")))
  expect_true(grepl("Multiple steps exist", t$fun("iris-Species", active = "true"), fixed = TRUE))

  # Unknown step / unknown filter.
  expect_true(grepl("not found", t$fun("iris-Species", active = "true", step_id = "9"), fixed = TRUE))
  expect_true(grepl("No matching filters", t$fun("nope", active = "true", step_id = "1"), fixed = TRUE))
})

# -- cb_tool_clear_filters -----------------------------------------------------

test_that("cb_tool_clear_filters resets all or selected filters", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  coh$add_step(step(
    filter("discrete", id = "iris-Species", dataset = "iris",
           variable = "Species", value = "setosa")
  ), run_flow = TRUE)
  t <- cb_tool_clear_filters(coh)

  # Clear all filters in the (single) step.
  expect_true(grepl("reset to defaults", t$fun(), fixed = TRUE))

  # Reset a specific filter by id.
  coh$update_filter("1", "iris-Species", value = "setosa", run_flow = TRUE)
  out <- t$fun("iris-Species")
  expect_true(grepl("Reset filters", out, fixed = TRUE))
})

test_that("cb_tool_clear_filters handles empty, multi-step and unknown cases", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_clear_filters(coh)

  expect_true(grepl("No steps configured", t$fun(), fixed = TRUE))

  coh$add_step(step(filter("discrete", id = "iris-Species", dataset = "iris", variable = "Species")))
  coh$add_step(step(filter("discrete", id = "mtcars-hp", dataset = "mtcars", variable = "hp")))
  expect_true(grepl("Multiple steps exist", t$fun(), fixed = TRUE))
  expect_true(grepl("not found", t$fun(step_id = "9"), fixed = TRUE))
  expect_true(grepl("No matching filters", t$fun("nope", step_id = "1"), fixed = TRUE))
})

# -- cb_tool_remove_filters ----------------------------------------------------

test_that("cb_tool_remove_filters removes filters from a step", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  coh$add_step(step(
    filter("discrete", id = "iris-Species", dataset = "iris", variable = "Species"),
    filter("range", id = "iris-SepalLength", dataset = "iris", variable = "Sepal.Length")
  ), run_flow = TRUE)
  t <- cb_tool_remove_filters(coh)

  out <- t$fun("iris-Species")
  expect_true(grepl("Removed filters", out, fixed = TRUE))
  expect_false("iris-Species" %in% names(coh$get_step("1")$filters))
})

test_that("cb_tool_remove_filters handles empty, multi-step and unknown cases", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_remove_filters(coh)

  expect_true(grepl("No steps configured", t$fun("iris-Species"), fixed = TRUE))

  coh$add_step(step(filter("discrete", id = "iris-Species", dataset = "iris", variable = "Species")))
  coh$add_step(step(filter("discrete", id = "mtcars-hp", dataset = "mtcars", variable = "hp")))
  expect_true(grepl("Multiple steps exist", t$fun("iris-Species"), fixed = TRUE))
  expect_true(grepl("not found", t$fun("iris-Species", step_id = "9"), fixed = TRUE))
  expect_true(grepl("No matching filters", t$fun("nope", step_id = "1"), fixed = TRUE))
})

# -- cb_tool_remove_step -------------------------------------------------------

test_that("cb_tool_remove_step removes the last step", {
  coh <- make_test_cohort()
  t <- cb_tool_remove_step(coh)

  expect_identical(t$fun(), "No steps to remove.")

  coh$add_step(step(filter("discrete", id = "iris-Species", dataset = "iris", variable = "Species")))
  coh$add_step(step(filter("discrete", id = "mtcars-hp", dataset = "mtcars", variable = "hp")))
  out <- t$fun()
  expect_true(grepl("Removed step 2", out, fixed = TRUE))
  expect_true(grepl("mtcars-hp", out, fixed = TRUE))
  expect_length(coh$get_step(), 1L)
})

# -- cb_tool_get_code ----------------------------------------------------------

test_that("cb_tool_get_code returns code or a helpful message", {
  coh <- make_test_cohort()
  t <- cb_tool_get_code(coh)
  expect_identical(t$name, "cb_get_code")

  expect_true(grepl("No steps configured", t$fun(), fixed = TRUE))

  coh$add_step(step(
    filter("discrete", id = "iris-Species", dataset = "iris",
           variable = "Species", value = "setosa")
  ), run_flow = TRUE)
  out <- t$fun()
  expect_type(out, "character")
  expect_true(nzchar(out))
})

# -- cb_tool_run ---------------------------------------------------------------

test_that("cb_tool_run reports auto-run when enabled", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_run(coh)
  withr::with_options(list(cb_tool_run_cohort = TRUE), {
    expect_true(grepl("run automatically", t$fun(), fixed = TRUE))
  })
})

test_that("cb_tool_run executes the pipeline when auto-run is disabled", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  coh$add_step(step(
    filter("discrete", id = "iris-Species", dataset = "iris",
           variable = "Species", value = "setosa")
  ))
  t <- cb_tool_run(coh)
  withr::with_options(list(cb_tool_run_cohort = FALSE), {
    expect_identical(t$fun(), "Cohort pipeline executed for all steps.")
    expect_true(grepl("executed", t$fun(step_id = "1"), fixed = TRUE))
    expect_true(grepl("not found", t$fun(step_id = "9"), fixed = TRUE))
  })
})

# -- Tool logging (cb_tool_verbose) --------------------------------------------

test_that("tools are silent unless cb_tool_verbose is TRUE", {
  coh <- make_test_cohort()
  t <- cb_tool_describe_state(coh)

  withr::with_options(list(cb_tool_verbose = FALSE), {
    expect_silent(t$fun())
  })
})

test_that("cb_tool_verbose emits an informative message with arguments", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  coh$add_step(step(
    filter("discrete", id = "iris-Species", dataset = "iris", variable = "Species")
  ))
  t <- cb_tool_toggle_filters(coh)

  withr::with_options(list(cb_tool_verbose = TRUE), {
    expect_message(
      t$fun("iris-Species", active = "false"),
      "cb_toggle_filters"
    )
    # Argument values are included in the log line.
    expect_message(
      t$fun("iris-Species", active = "true"),
      "iris-Species"
    )
  })
})

test_that("cb_tool_log renders absent arguments as <none>", {
  withr::with_options(list(cb_tool_verbose = TRUE), {
    expect_message(cb_tool_log("cb_test", step_id = NULL), "step_id = <none>", fixed = TRUE)
  })
})
