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
  expect_true(any(grepl("my_tool", out)))
  expect_true(any(grepl("Does something useful", out)))
  expect_true(any(grepl("x, y", out)))
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
  expect_true("Species" %in% names(parsed$filters))
  species <- parsed$filters$Species
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
  expect_true(grepl("No filters metadata", result))
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

  result <- t$fun("Species, hp", action = "new_step")
  expect_true(grepl("Species", result))
  expect_true(grepl("hp", result))
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
  result <- t$fun("hp", action = "edit_last")
  expect_true(grepl("hp", result))
  # Still step 1, filter was added to it
  expect_identical(coh$last_step_id(), "1")
  filters <- coh$get_step("1")$filters
  expect_true("hp" %in% names(filters))
})

test_that("cb_tool_add_filters skips filters already present in the step", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)

  t$fun("Species", action = "new_step")
  result <- t$fun("Species", action = "edit_last")

  expect_true(grepl("Already present in step \\(skipped\\)", result))
  expect_true(grepl("Species", result))
  # No duplicate added: still a single Species filter.
  expect_identical(sum(names(coh$get_step("1")$filters) == "Species"), 1L)
})

test_that("cb_tool_apply_filters updates values of existing filters in place", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_apply_filters(coh)

  t$fun('{"Species":{"value":["setosa"]}}', action = "new_step")
  result <- t$fun('{"Species":{"value":["versicolor"]}}', action = "edit_last")

  expect_true(grepl("Updated values for existing filters", result))
  # Value updated in place, filter not reset or duplicated.
  filters <- coh$get_step(coh$last_step_id())$filters
  expect_identical(filters[["Species"]]@value, "versicolor")
  expect_identical(sum(names(filters) == "Species"), 1L)
})

test_that("cb_tool_add_filters can add filters as inactive", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)

  result <- t$fun("Species", action = "new_step", active = "false")
  expect_true(grepl("inactive", result))
  expect_false(coh$get_step("1")$filters[["Species"]]@active)
})

test_that("cb_tool_add_filters inherits active state when active is omitted", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  # Force the available Species filter to be inactive by default.
  src <- coh$get_source()
  src$available_filters <- purrr::map(src$available_filters, function(f) {
    if (f@id == "Species") f@active <- FALSE
    f
  })
  t <- cb_tool_add_filters(coh)

  result <- t$fun("Species", action = "new_step")
  expect_false(grepl("active|inactive", result))
  expect_false(coh$get_step("1")$filters[["Species"]]@active)
})

test_that("cb_tool_apply_filters can add filters as inactive", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_apply_filters(coh)

  result <- t$fun('{"Species":{"value":["setosa"]}}', action = "new_step", active = "false")
  expect_true(grepl("inactive", result))
  flt <- coh$get_step(coh$last_step_id())$filters[["Species"]]
  expect_false(flt@active)
  expect_identical(flt@value, "setosa")
})

test_that("cb_tool_add_filters reports unknown filter ids", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)
  result <- t$fun("Species, nonexistent_filter")
  expect_true(grepl("Unknown filter ids ignored", result))
  expect_true(grepl("nonexistent_filter", result))
})

test_that("cb_tool_add_filters handles all unknown ids", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_add_filters(coh)
  result <- t$fun("fake_one, fake_two")
  expect_true(grepl("No filters found matching", result))
})

test_that("cb_tool_add_filters handles no available_filters", {
  skip_if_not_installed("ellmer")
  source <- set_source(tblist(iris = iris))
  coh <- cohort(source = source)
  t <- cb_tool_add_filters(coh)
  result <- t$fun("Species")
  expect_true(grepl("No available filters", result))
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
  expect_true(grepl("Invalid JSON", result))
})

test_that("cb_tool_set_filter_values handles no steps", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  t <- cb_tool_set_filter_values(coh)
  result <- t$fun('{"Species": {"value": ["setosa"]}}')
  expect_true(grepl("No steps", result))
})

# -- Registration functions ----------------------------------------------------

test_that("cb_register_tool registers tool with chat", {
  skip_if_not_installed("ellmer")
  coh <- make_test_cohort()
  chat <- MockChat$new()
  t <- cb_tool_filters_meta(coh)
  expect_invisible(cb_register_tool(chat, t))
  expect_identical(length(chat$tools), 1L)
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
  expect_identical(length(chat$tools), 12L)
})
