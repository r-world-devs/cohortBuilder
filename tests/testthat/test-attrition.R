test_that("get_attrition_coords returns a data frame with expected structure and values", {
  # Example input
  labels <- c("x", "Step: 1\nFilter: adsa (range = [1, 4])")
  n_included <- c(`0` = 150L, `1` = 100L)
  space <- 1L
  percent <- FALSE
  # Call the function

  result <- get_attrition_coords(labels = labels, n_included = n_included, space = space, percent = percent)

  # Basic structure tests
  expect_s3_class(result, "data.frame")

  # Check number of rows is equal to length of labels
  expect_identical(nrow(result), length(labels))

  # Check that space attribute is retained correctly
  expect_identical(attr(result, "space"), space)

  # Check label formatting without percent
  # Construct expected label without percentage
  expected_label <- c(
    "x\nN = 150",
    "Step: 1\nFilter: adsa (range = [1, 4])\nN = 100"
  )

  # The exclusion label should have NA in the first row and the computed exclusion
  # in the second row (excluded = 150 - 100 = 50)
  expected_label_excl <- c(
    NA,
    "Excluded N = 50"
  )

  expect_identical(result$label, expected_label)
  expect_identical(result$label_excl, expected_label_excl)

  # Check a few numeric values to ensure calculations are done as expected
  # label_heights counts the number of newline chars in `label`.
  # "x\nN = 150" has 1 newline, so label_heights[1] should be 1.
  # "Step: 1\nFilter: adsa (range = [1, 4])\nN = 100" has 2 newlines, so label_heights[2] should be 2.
  expect_identical(result$label_heights, c(1L, 2L))

  # The positioning should start at 0 and accumulate by label_heights + space.
  # For the first row: label_position_y = 0.
  # For the second row: label_position_y = previous (0) + (1 + 1) = 2.
  expect_identical(result$label_position_y, c(0L, 2L))

  # arrow_end_position_y for the first row should lead to the second row's position, i.e., 2.
  # For the second row, since there is no next row, it should be NA.
  expect_identical(result$arrow_end_position_y, c(2L, NA_integer_))

  # excl_position_y for the first row is calculated using lag(arrow_end_position_y) - (space/2) = NA
  # since lag of first is NA.
  # For the second row: excl_position_y = previous arrow_end_position_y (2) - (1/2) = 1.5
  expect_identical(result$excl_position_y, c(NA_real_, 1.5))
})

test_that("get_attrition_coords calculates percentages correctly when percent = TRUE", {
  labels <- c("A", "B")
  n_included <- c(`0` = 200L, `1` = 100L)
  space <- 1L
  percent <- TRUE

  result <- get_attrition_coords(labels = labels, n_included = n_included, space = space, percent = percent)

  # Expected percentage: for label, 100 * (n_included/n_total)
  # Row 1: 200/200 * 100 = 100%
  # Row 2: 100/200 * 100 = 50%
  expected_label <- c(
    "A\nN = 200 (100%)",
    "B\nN = 100 (50%)"
  )

  # Excluded for row 2 is 200 - 100 = 100
  # Percentage: 100/200 * 100 = 50%
  expected_label_excl <- c(
    NA,
    "Excluded N = 100 (50%)"
  )

  expect_identical(result$label, expected_label)
  expect_identical(result$label_excl, expected_label_excl)
})

test_that("get_attrition_plot returns a ggplot object with expected structure", {
  # Using the provided input data from get_attrition_coords
  labels <- c("x", "Step: 1\nFilter: adsa (range = [1, 4])")
  n_included <- c(`0` = 150L, `1` = 100L)
  space <- 1L
  percent <- FALSE
  input_data <- get_attrition_coords(labels = labels, n_included = n_included, space = space, percent = percent)

  plot <- get_attrition_plot(input_data)

  # Check that the result is a ggplot object
  expect_s3_class(plot, "ggplot")

  # The logic of the function involves reversing the y-axis scale.
  # Check that a reversed y scale is applied.
  y_scale <- plot$scales$get_scales("y")
  expect_identical(y_scale$trans$name, "reverse", info = "Y scale should be reversed.")

  # Confirm that the labs are set to NULL for x and y, as per the logic
  expect_null(plot$labels$x)
  expect_null(plot$labels$y)
})
