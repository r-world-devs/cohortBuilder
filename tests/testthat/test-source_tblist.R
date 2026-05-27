test_that("tblist throws error when input is not data frames or list of data frames", {
  expect_error(tblist(NULL), "Please provide a data.frame or list of data.frames")
  expect_error(tblist(123L), "Please provide a data.frame or list of data.frames")
  expect_error(tblist("123"), "Please provide a data.frame or list of data.frames")
  expect_error(tblist(1.5), "Please provide a data.frame or list of data.frames")
  expect_error(tblist(TRUE), "Please provide a data.frame or list of data.frames")
})

test_that("tblist applies names correctly to list of data frames", {
  df_list <- list(mtcars, iris)
  result <- tblist(df_list, names = c("Cars", "Flowers"))

  result_without_names <- tblist(df_list)

  expect_s3_class(result, "tblist")
  expect_named(result, c("Cars", "Flowers"))
  expect_identical(result$Cars, mtcars)
  expect_identical(result$Flowers, iris)
})

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
  expect_false(anyNA(result$test_dataset$var1))
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
  expect_type(result, "integer")
})

test_that("get_params in discrete filter works fine", {
  filter <- cb_filter.discrete.tblist(
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  expect_type(filter$get_params("value"), "character")
  expect_type(filter$get_params(), "list")
})

test_that("get_data in discrete filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  test_data_null <- list(
    test_dataset = data.frame(
      var1 = NULL
    )
  )

  filter <- cb_filter.discrete.tblist(
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  # Filter with non-existing variable
  wrong_filter <- cb_filter.discrete.tblist(
    variable = "non-existing", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  expect_type(filter$get_data(test_data), "character")
  expect_length(filter$get_data(test_data), length(test_var))
  expect_null(wrong_filter$get_data(test_data))
  expect_null(filter$get_data(test_data_null))
})

test_that("get_defaults in discrete filter works fine", {
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

  result <- filter$get_defaults(test_data, filter$get_stats(test_data))

  expect_type(result, "list")
  expect_length(result, 1L)
  expect_type(result$value, "character")
  expect_length(result$value, length(test_var %>% na.omit() %>% collapse::funique()))
  expect_identical(result$value, as.vector(test_var %>% na.omit() %>% collapse::funique()))
})

test_that("get_stats in discrete text filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  filter <- cb_filter.discrete_text.tblist(
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$get_stats(test_data, "n_missing")
  expect_identical(result, length(test_var[is.na(test_var)]))
  expect_type(result, "integer")
})

test_that("get_params in discrete text filter works fine", {
  filter <- cb_filter.discrete_text.tblist(
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  expect_type(filter$get_params("value"), "character")
  expect_type(filter$get_params(), "list")
})

test_that("get_data in discrete text filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  test_data_null <- list(
    test_dataset = data.frame(
      var1 = NULL
    )
  )

  filter <- cb_filter.discrete_text.tblist(
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  # Filter with non-existing variable
  wrong_filter <- cb_filter.discrete_text.tblist(
    variable = "non-existing", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  expect_type(filter$get_data(test_data), "character")
  expect_length(filter$get_data(test_data), length(test_var))
  expect_null(wrong_filter$get_data(test_data))
  expect_null(filter$get_data(test_data_null))
})

test_that("get_defaults in discrete text filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  filter <- cb_filter.discrete_text.tblist(
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$get_defaults(test_data, filter$get_stats(test_data))

  expect_type(result, "list")
  expect_length(result, 1L)
  expect_type(result$value, "character")
  expect_length(result$value, 1L)
})

test_that("get_range_frequencies with empty data works fine", {
  test_data_null <- list(
    test_dataset = data.frame(
      var1 = NULL
    )
  )

  result <- get_range_frequencies(test_data_null, "test_dataset", "var1")

  expect_type(result, "list")
  expect_identical(nrow(result), 0L)
  expect_identical(ncol(result), 4L)
})

test_that("filter_data in range filter works fine", {
  test_var <- c(42L, 7L, 89L, NA, 16L, 73L, 58L, 91L, 35L, NA, 24L, 67L)

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  # filter_data with keep_na = TRUE and value != NA
  filter <- cb_filter.range.tblist(
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$filter_data(test_data)

  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_type(result$test_dataset$var1, "integer")
  expect_gt(length(result$test_dataset$var1), 0L)

  # filter_data with keep_na = FALSE and value = NA
  filter2 <- cb_filter.range.tblist(
    variable = "var1", range = NA,
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- filter2$filter_data(test_data)
  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_type(result$test_dataset$var1, "integer")
  expect_length(result$test_dataset$var1, length(test_var %>% na.omit()))
  expect_false(anyNA(result$test_dataset$var1))

  # filter_data with keep_na = FALSE and value != NA
  filter3 <- cb_filter.range.tblist(
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- filter3$filter_data(test_data)
  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_type(result$test_dataset$var1, "integer")
  expect_false(anyNA(result$test_dataset$var1))
})

test_that("get_stats in range filter works fine", {
  test_var <- c(42L, 7L, 89L, NA, 16L, 73L, 58L, 91L, 35L, NA, 24L, 67L)

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  filter <- cb_filter.range.tblist(
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$get_stats(test_data, "n_data")

  expect_type(result, "integer")
  expect_length(result, 1L)
  expect_identical(result, length(test_var %>% na.omit()))
})

test_that("get_params in range filter works fine", {
  filter <- cb_filter.range.tblist(
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$get_params("variable")
  expect_type(result, "character")
  expect_length(result, 1L)

  result2 <- filter$get_params()

  expect_type(result2, "list")
  expect_type(result2$dataset, "character")
  expect_identical(result, result2$variable)
})

test_that("get_data in range filter works fine", {
  test_var <- c(42L, 7L, 89L, NA, 16L, 73L, 58L, 91L, 35L, NA, 24L, 67L)

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  filter <- cb_filter.range.tblist(
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$get_data(test_data)

  expect_type(result, "integer")
  expect_identical(result, test_var)
})

test_that("get_defaults in range filter works fine", {
  test_var <- c(42L, 7L, 89L, NA, 16L, 73L, 58L, 91L, 35L, NA, 24L, 67L)

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  filter <- cb_filter.range.tblist(
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$get_defaults(test_data, filter$get_stats(test_data))
  expect_type(result, "list")
  expect_length(result$range, 2L)
})

test_that("get_date_range_frequencies works fine", {
  test_var <- c(
    "2026-01-01", "2022-01-02", "2021-01-02",
    "2024-01-03", "2024-01-05", NA
  )
  test_data <- list(
    test_dataset = data.frame(
      var1 = as.Date(test_var)
    )
  )

  test_data_with_one_date <- list(
    test_dataset = data.frame(
      var1 = as.Date("2024-01-03")
    )
  )

  test_data_null <- list(
    test_dataset = data.frame(
      var1 = NULL
    )
  )

  # With empty data
  result <- get_date_range_frequencies(test_data_null, "test_dataset", "var1")

  expect_type(result, "list")
  expect_identical(nrow(result), 0L)
  expect_identical(ncol(result), 4L)

  # With data and multi dates
  result <- get_date_range_frequencies(test_data, "test_dataset", "var1", extra_params = NULL)

  expect_identical(ncol(result), 4L)
  expect_type(result, "list")
  expect_s3_class(result$l_bound, "Date")
  expect_s3_class(result$u_bound, "Date")

  # With data and one date
  result <- get_date_range_frequencies(test_data_with_one_date, "test_dataset", "var1", extra_params = NULL)

  expect_identical(ncol(result), 4L)
  expect_identical(nrow(result), 1L)
  expect_type(result, "list")
  expect_s3_class(result$l_bound, "Date")
  expect_s3_class(result$u_bound, "Date")
})

test_that("filter_data in date range filter works fine", {
  test_var <- c(
    "2026-01-01", "2022-01-02", "2021-01-02",
    "2024-01-03", "2024-01-05", NA
  )

  test_data <- list(
    test_dataset = data.frame(
      var1 = as.Date(test_var)
    )
  )

  # filter_data with keep_na = TRUE and value != NA
  filter <- cb_filter.date_range.tblist(
    variable = "var1", range = as.Date(c("2021-01-02", "2024-01-03")),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- filter$filter_data(test_data)

  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_s3_class(result$test_dataset$var1, "Date")

  # filter_data with keep_na = FALSE and value = NA
  filter2 <- cb_filter.date_range.tblist(
    variable = "var1", range = NA,
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- filter2$filter_data(test_data)
  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_s3_class(result$test_dataset$var1, "Date")
  expect_false(anyNA(result$test_dataset$var1))

  # filter_data with keep_na = FALSE and value != NA
  filter3 <- cb_filter.date_range.tblist(
    variable = "var1", range = as.Date(c("2021-01-02", "2024-01-03")),
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- filter3$filter_data(test_data)
  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_s3_class(result$test_dataset$var1, "Date")
  expect_false(anyNA(result$test_dataset$var1))
})

test_that("get_stats in date range filter works fine", {
  test_var <- c(
    "2026-01-01", "2022-01-02", "2021-01-02",
    "2024-01-03", "2024-01-05", NA
  )

  test_data <- list(
    test_dataset = data.frame(
      var1 = as.Date(test_var)
    )
  )

  filter <- cb_filter.date_range.tblist(
    variable = "var1", range = as.Date(c("2021-01-02", "2024-01-03")),
    dataset = "test_dataset", keep_na = TRUE
  )

  # All stats
  result <- filter$get_stats(test_data)

  expect_type(result, "list")
  expect_type(result$n_data, "integer")
  expect_s3_class(result$frequencies, "data.frame")
  expect_type(result$n_missing, "integer")

  # One stat
  expect_type(filter$get_stats(test_data, "n_data"), "integer")
  expect_identical(filter$get_stats(test_data, "n_data"), result$n_data)
})

test_that("plot_data in date range filter works fine", {
  test_var <- c(
    "2026-01-01", "2022-01-02", "2021-01-02",
    "2024-01-03", "2024-01-05", NA
  )

  test_data <- list(
    test_dataset = data.frame(
      var1 = as.Date(test_var)
    )
  )

  test_data_null <- list(
    test_dataset = data.frame(
      var1 = NULL
    )
  )

  filter <- cb_filter.date_range.tblist(
    variable = "var1", range = as.Date(c("2021-01-02", "2024-01-03")),
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "date_range - breaks argument is passed properly",
    fig = function() {
      filter$plot_data(test_data, breaks = "year")
    }
  )
  vdiffr::expect_doppelganger(
    "date_range - Extra args work",
    fig = function() {
      filter$plot_data(test_data, freq = TRUE, breaks = "year")
    }
  )
  vdiffr::expect_doppelganger(
    "date_range - No data case works",
    fig = function() {
      filter$plot_data(test_data_null, breaks = "year")
    }
  )
})

test_that("plot_data in range filter works fine", {
  test_var <- c(seq(-2L, 2L, length.out = 10L), NA)
  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  test_data_null <- list(
    test_dataset = data.frame(
      var1 = NULL
    )
  )

  filter <- cb_filter.range.tblist(
    variable = "var1", range = c(-1L, 1L),
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "range - Breaks argument is passed properly",
    fig = function() {
      filter$plot_data(test_data, breaks = 10L)
    }
  )
  vdiffr::expect_doppelganger(
    "range - Extra args work",
    fig = function() {
      filter$plot_data(test_data, breaks = 5L, freq = TRUE)
    }
  )
  vdiffr::expect_doppelganger(
    "range - No data case works",
    fig = function() {
      filter$plot_data(test_data_null, breaks = c(-1L, 0L, 1L))
    }
  )
})

test_that("plot_data in discrete filter works fine", {
  test_var <- c("a", "a", "b", "b", "b", "c", NA)
  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  test_data_null <- list(
    test_dataset = data.frame(
      var1 = NULL
    )
  )

  filter <- cb_filter.discrete.tblist(
    variable = "var1", value = c("b", "c"),
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "discrete - Extra args work",
    fig = function() {
      filter$plot_data(test_data, axes = FALSE)
    }
  )
  vdiffr::expect_doppelganger(
    "discrete - No data case works",
    fig = function() {
      filter$plot_data(test_data_null)
    }
  )
})


test_that("plot_data in datetime_range filter works fine", {
  test_var <- as.POSIXct("2026-02-24 10:34:44 UTC") + 360L * 1L:20L
  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  test_data_null <- list(
    test_dataset = data.frame(
      var1 = NULL
    )
  )

  filter <- cb_filter.datetime_range.tblist(
    variable = "var1", range = NA,
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "datetime_range - default breaks work", fig = function() {
      filter$plot_data(test_data)
    }
  )
  vdiffr::expect_doppelganger(
    "datetime_range - Breaks arg works",
    fig = function() {
      filter$plot_data(test_data, breaks = "hours")
    }
  )
  vdiffr::expect_doppelganger(
    "datetime_range - Extra args work",
    fig = function() {
      filter$plot_data(test_data, freq = TRUE, breaks = "hours")
    }
  )
  vdiffr::expect_doppelganger(
    "datetime_range - No data case works", fig = function() {
      filter$plot_data(test_data_null)
    }
  )
})

test_that("plot_data in multi_discrete filter works fine", {
  test_var1 <- c("a", "a", "b", "b", "b", "c")
  test_var2 <- c("A", "A", "C", "A", "A", "B")
  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var1,
      var2 = test_var2
    )
  )

  test_data_null <- list(
    test_dataset = data.frame(
      var1 = NULL
    )
  )

  filter <- cb_filter.multi_discrete.tblist(
    variables = c("var1", "var2"), value = NA,
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "multi_discrete - standard call works",
    fig = function() {
      filter$plot_data(test_data)
    }
  )

  vdiffr::expect_doppelganger(
    "multi_discrete - Extra args work",
    fig = function() {
      filter$plot_data(test_data, axes = FALSE)
    }
  )

  vdiffr::expect_doppelganger(
    "multi_discrete - No data case works",
    fig = function() {
      filter$plot_data(test_data_null)
    }
  )
})
