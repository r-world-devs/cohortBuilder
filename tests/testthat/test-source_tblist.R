test_that("tblist returns named list of data frames when provided unnamed data frames", {
  result <- tblist(mtcars, iris)

  expect_s3_class(result, "tblist")
  expect_type(result, "list")
  expect_named(result, c("mtcars", "iris"))
})

test_that("tblist returns named list of data frames when provided named arguments", {
  result <- tblist(MT = mtcars, IR = iris)

  expect_s3_class(result, "tblist")
  expect_named(result, c("MT", "IR"))
  expect_identical(result$MT, mtcars)
  expect_identical(result$IR, iris)
})

test_that("tblist uses custom names when 'names' argument is provided", {
  result <- tblist(mtcars, iris, names = c("CarData", "FlowerData"))

  expect_s3_class(result, "tblist")
  expect_named(result, c("CarData", "FlowerData"))
  expect_identical(result$CarData, mtcars)
  expect_identical(result$FlowerData, iris)
})

test_that("tblist throws error when 'names' length does not match input length", {
  expect_error(tblist(mtcars, iris, names = c("OnlyOneName")),
               "should be of same length as 'names'")
  expect_error(tblist(mtcars, names = c("FirstName", "SecondName")),
               "should be of same length as 'names'")
  expect_error(tblist(list(mtcars), names = c("FirstName", "SecondName")),
               "should be of same length as 'names'")
})

test_that("tblist handles empty input correctly", {
  result <- tblist()
  expect_s3_class(result, "tblist")
  expect_length(result, 0L)
})

test_that("as.tblist works fine", {
  expect_s3_class(as.tblist(list(iris)), "tblist")
  expect_s3_class(as.tblist(iris), "tblist")
})

test_that("filter_data in discrete filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  # filter_data with keep_na = TRUE and value != NA
  filter <- cb_filter.discrete.tblist(
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$filter_data(test_data)
  expect_type(result$test_dataset$var1, "character")
  expect_length(result$test_dataset$var1, length(test_var %>% .[. == "A"]))
  expect_setequal(result$test_dataset$var1, c("A", NA))

  # filter_data with keep_na = FALSE and value = NA
  filter2 <- cb_filter.discrete.tblist(
    variable = "var1", value = NA,
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- filter2$filter_data(test_data)
  expect_type(result$test_dataset$var1, "character")
  expect_length(result$test_dataset$var1, length(test_var %>% na.omit()))
  expect_false(anyNA(result$test_dataset$var1))

  # filter_data with keep_na = FALSE and value != NA
  filter3 <- cb_filter.discrete.tblist(
    variable = "var1", value = "B",
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- filter3$filter_data(test_data)
  expect_type(result$test_dataset$var1, "character")
  expect_length(result$test_dataset$var1, length(test_var %>% na.omit() %>% .[. == "B"]))
  expect_setequal(result$test_dataset$var1, "B")
})

test_that("get_stats in discrete filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  filter <- cb_filter.discrete.tblist(
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$get_stats(test_data, "n_data")
  expect_identical(result, length(test_var %>% na.omit()))
})

test_that("plot_data in discrete filter works fine", {
  test_var <- NULL

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  filter <- cb_filter.discrete.tblist(
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )
  filter$plot_data(test_data)
  expect_silent(recordPlot())
})

