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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  # filter_data with keep_na = TRUE and value != NA
  filter_obj <- filter("discrete",
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- cb_filter_data(filter_obj, source, test_data)
  expect_type(result$test_dataset$var1, "character")
  expect_length(result$test_dataset$var1, sum(test_var == "A" | is.na(test_var), na.rm = TRUE))
  expect_setequal(result$test_dataset$var1, c("A", NA))

  # filter_data with keep_na = FALSE and value = NA
  filter_obj2 <- filter("discrete",
    variable = "var1", value = NA,
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- cb_filter_data(filter_obj2, source, test_data)
  expect_type(result$test_dataset$var1, "character")
  expect_length(result$test_dataset$var1, length(test_var |> na.omit()))
  expect_false(anyNA(result$test_dataset$var1))

  # filter_data with keep_na = FALSE and value != NA
  filter_obj3 <- filter("discrete",
    variable = "var1", value = "B",
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- cb_filter_data(filter_obj3, source, test_data)
  expect_type(result$test_dataset$var1, "character")
  expect_length(result$test_dataset$var1, sum(test_var == "B", na.rm = TRUE))
  expect_setequal(result$test_dataset$var1, "B")
  expect_false(anyNA(result$test_dataset$var1))
})

test_that("discrete filter handles a factor-valued effective value", {
  # Regression: a factor-valued domain/value must not silently drop rows.
  # `c(factor, NA)` coerces the factor to integer codes, so `column %in% value`
  # would match nothing. This reproduces what domain propagation produced for a
  # factor column in a downstream step (post-stats collapsing to 0 / 0%).
  test_var <- factor(c("A", "B", "C", "B", "C"), levels = c("A", "B", "C"))
  test_data <- list(test_dataset = data.frame(var1 = test_var))
  source <- set_source(do.call(tblist, test_data))

  filter_obj <- filter("discrete",
    variable = "var1", dataset = "test_dataset", keep_na = TRUE,
    # value carries the factor's levels, mimicking a propagated factor domain
    value = factor(c("B", "C"), levels = c("A", "B", "C"))
  )

  result <- cb_filter_data(filter_obj, source, test_data)
  expect_length(result$test_dataset$var1, sum(test_var %in% c("B", "C")))
  expect_setequal(as.character(result$test_dataset$var1), c("B", "C"))
})

test_that("get_stats in discrete filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("discrete",
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- cb_get_filter_stats(filter_obj, source, test_data, name = "n_data")
  expect_identical(result, length(test_var |> na.omit()))
  expect_type(result, "integer")
})

test_that("get_params in discrete filter works fine", {
  filter_obj <- filter("discrete",
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  expect_type(get_filter_params(filter_obj, "value"), "character")
  expect_type(get_filter_params(filter_obj), "list")
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("discrete",
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  # Filter with non-existing variable
  wrong_filter <- filter("discrete",
    variable = "non-existing", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  expect_type(cb_get_filter_data(filter_obj, source, test_data), "character")
  expect_length(cb_get_filter_data(filter_obj, source, test_data), length(test_var))
  expect_null(cb_get_filter_data(wrong_filter, source, test_data))
  expect_null(cb_get_filter_data(filter_obj, source, test_data_null))
})

test_that("get_defaults in discrete filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("discrete",
    variable = "var1", value = "A",
    dataset = "test_dataset", keep_na = TRUE
  )

  stats <- cb_get_filter_stats(filter_obj, source, test_data)
  result <- cb_get_filter_defaults(filter_obj, source, test_data, stats)

  expect_type(result, "list")
  expect_length(result, 1L)
  expect_type(result$value, "character")
  expect_length(result$value, length(test_var |> na.omit() |> collapse::funique()))
  expect_identical(result$value, as.vector(test_var |> na.omit() |> collapse::funique()))
})

test_that("get_stats in discrete text filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("discrete_text",
    variable = "var1", value = "A",
    dataset = "test_dataset"
  )

  result <- cb_get_filter_stats(filter_obj, source, test_data, name = "n_missing")
  expect_identical(result, length(test_var[is.na(test_var)]))
  expect_type(result, "integer")
})

test_that("get_params in discrete text filter works fine", {
  filter_obj <- filter("discrete_text",
    variable = "var1", value = "A",
    dataset = "test_dataset"
  )

  expect_type(get_filter_params(filter_obj, "value"), "character")
  expect_type(get_filter_params(filter_obj), "list")
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("discrete_text",
    variable = "var1", value = "A",
    dataset = "test_dataset"
  )

  # Filter with non-existing variable
  wrong_filter <- filter("discrete_text",
    variable = "non-existing", value = "A",
    dataset = "test_dataset"
  )

  expect_type(cb_get_filter_data(filter_obj, source, test_data), "character")
  expect_length(cb_get_filter_data(filter_obj, source, test_data), length(test_var))
  expect_null(cb_get_filter_data(wrong_filter, source, test_data))
  expect_null(cb_get_filter_data(filter_obj, source, test_data_null))
})

test_that("get_defaults in discrete text filter works fine", {
  test_var <- c("A", "B", NA, "C", "A", NA, "B")

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("discrete_text",
    variable = "var1", value = "A",
    dataset = "test_dataset"
  )

  stats <- cb_get_filter_stats(filter_obj, source, test_data)
  result <- cb_get_filter_defaults(filter_obj, source, test_data, stats)

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

test_that("get_range_frequencies caps breaks for wide numeric ranges", {
  # Regression: a fixed step of 1 over a very wide span (e.g. a population
  # column spanning 0..1e12) made seq(min, max, by = 1) request a trillion-
  # element vector and error with "'by' argument is much too small". The break
  # count must instead be capped by widening the step.
  wide <- list(t = data.frame(v = c(0L, 5e5L, 1e12)))
  result <- expect_no_error(
    get_range_frequencies(wide, "t", "v", extra_params = NULL)
  )
  expect_lte(nrow(result), getOption("cb_range_stats_max_breaks", 1000L) + 1L)
  # The full data span is preserved and every observation is counted.
  expect_identical(result$l_bound[1L], 0.0)
  expect_identical(rev(result$u_bound)[1L], 1e12)
  expect_identical(sum(result$count), 3L)

  # An explicit step is still honoured verbatim.
  stepped <- get_range_frequencies(wide, "t", "v", extra_params = list(step = 1e11))
  expect_identical(stepped$l_bound[2L] - stepped$l_bound[1L], 1e11)
})

test_that("get_range_frequencies keeps step 1 for small integer ranges", {
  # Backward compatibility: narrow ranges must be unaffected by the cap.
  small <- list(t = data.frame(v = c(7L, 24L, 42L, 91L)))
  result <- get_range_frequencies(small, "t", "v", extra_params = NULL)
  expect_identical(result$l_bound[2L] - result$l_bound[1L], 1.0)
  expect_identical(sum(result$count), 4L)
})

test_that("filter_data in range filter works fine", {
  test_var <- c(42L, 7L, 89L, NA, 16L, 73L, 58L, 91L, 35L, NA, 24L, 67L)

  test_data <- list(
    test_dataset = data.frame(
      var1 = test_var
    )
  )

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  # filter_data with keep_na = TRUE and value != NA
  filter_obj <- filter("range",
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- cb_filter_data(filter_obj, source, test_data)

  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_type(result$test_dataset$var1, "integer")
  expect_gt(length(result$test_dataset$var1), 0L)

  # filter_data with keep_na = FALSE and value = NA
  filter_obj2 <- filter("range",
    variable = "var1", range = NA,
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- cb_filter_data(filter_obj2, source, test_data)
  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_type(result$test_dataset$var1, "integer")
  expect_length(result$test_dataset$var1, length(test_var |> na.omit()))
  expect_false(anyNA(result$test_dataset$var1))

  # filter_data with keep_na = FALSE and value != NA
  filter_obj3 <- filter("range",
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- cb_filter_data(filter_obj3, source, test_data)
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("range",
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- cb_get_filter_stats(filter_obj, source, test_data, name = "n_data")

  expect_type(result, "integer")
  expect_length(result, 1L)
  expect_identical(result, length(test_var |> na.omit()))
})

test_that("get_params in range filter works fine", {
  filter_obj <- filter("range",
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- get_filter_params(filter_obj, "variable")
  expect_type(result, "character")
  expect_length(result, 1L)

  result2 <- get_filter_params(filter_obj)

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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("range",
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- cb_get_filter_data(filter_obj, source, test_data)

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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("range",
    variable = "var1", range = c(1L, 40L),
    dataset = "test_dataset", keep_na = TRUE
  )

  stats <- cb_get_filter_stats(filter_obj, source, test_data)
  result <- cb_get_filter_defaults(filter_obj, source, test_data, stats)
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

test_that("get_date_range_frequencies caps breaks for wide date ranges", {
  # A daily step over a multi-millennium span would create an unusably large
  # (and potentially erroring) number of breaks. The break count must be capped
  # by widening the step, mirroring the numeric case.
  wide <- list(
    t = data.frame(v = as.Date(c("0001-01-01", "2024-01-01", "5000-01-01")))
  )
  result <- expect_no_error(
    get_date_range_frequencies(wide, "t", "v", extra_params = NULL)
  )
  expect_lte(nrow(result), getOption("cb_range_stats_max_breaks", 1000L) + 1L)
  expect_s3_class(result$l_bound, "Date")
  expect_identical(sum(result$count), 3L)
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = as.Date(test_var))))

  # filter_data with keep_na = TRUE and value != NA
  filter_obj <- filter("date_range",
    variable = "var1", range = as.Date(c("2021-01-02", "2024-01-03")),
    dataset = "test_dataset", keep_na = TRUE
  )

  result <- cb_filter_data(filter_obj, source, test_data)

  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_s3_class(result$test_dataset$var1, "Date")

  # filter_data with keep_na = FALSE and value = NA
  filter_obj2 <- filter("date_range",
    variable = "var1", range = NA,
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- cb_filter_data(filter_obj2, source, test_data)
  expect_type(result, "list")
  expect_type(result$test_dataset, "list")
  expect_s3_class(result$test_dataset$var1, "Date")
  expect_false(anyNA(result$test_dataset$var1))

  # filter_data with keep_na = FALSE and value != NA
  filter_obj3 <- filter("date_range",
    variable = "var1", range = as.Date(c("2021-01-02", "2024-01-03")),
    dataset = "test_dataset", keep_na = FALSE
  )

  result <- cb_filter_data(filter_obj3, source, test_data)
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = as.Date(test_var))))

  filter_obj <- filter("date_range",
    variable = "var1", range = as.Date(c("2021-01-02", "2024-01-03")),
    dataset = "test_dataset", keep_na = TRUE
  )

  # All stats
  result <- cb_get_filter_stats(filter_obj, source, test_data)

  expect_type(result, "list")
  expect_type(result$n_data, "integer")
  expect_s3_class(result$frequencies, "data.frame")
  expect_type(result$n_missing, "integer")

  # One stat
  expect_type(cb_get_filter_stats(filter_obj, source, test_data, name = "n_data"), "integer")
  expect_identical(cb_get_filter_stats(filter_obj, source, test_data, name = "n_data"), result$n_data)
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = as.Date(test_var))))

  filter_obj <- filter("date_range",
    variable = "var1", range = as.Date(c("2021-01-02", "2024-01-03")),
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "date_range - breaks argument is passed properly",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data, breaks = "year")
    }
  )
  vdiffr::expect_doppelganger(
    "date_range - Extra args work",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data, freq = TRUE, breaks = "year")
    }
  )
  vdiffr::expect_doppelganger(
    "date_range - No data case works",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data_null, breaks = "year")
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("range",
    variable = "var1", range = c(-1L, 1L),
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "range - Breaks argument is passed properly",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data, breaks = 10L)
    }
  )
  vdiffr::expect_doppelganger(
    "range - Extra args work",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data, breaks = 5L, freq = TRUE)
    }
  )
  vdiffr::expect_doppelganger(
    "range - No data case works",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data_null, breaks = c(-1L, 0L, 1L))
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("discrete",
    variable = "var1", value = c("b", "c"),
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "discrete - Extra args work",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data, axes = FALSE)
    }
  )
  vdiffr::expect_doppelganger(
    "discrete - No data case works",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data_null)
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var)))

  filter_obj <- filter("datetime_range",
    variable = "var1", range = NA,
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "datetime_range - default breaks work", fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data)
    }
  )
  vdiffr::expect_doppelganger(
    "datetime_range - Breaks arg works",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data, breaks = "hours")
    }
  )
  vdiffr::expect_doppelganger(
    "datetime_range - Extra args work",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data, freq = TRUE, breaks = "hours")
    }
  )
  vdiffr::expect_doppelganger(
    "datetime_range - No data case works", fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data_null)
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

  source <- set_source(tblist(test_dataset = data.frame(var1 = test_var1, var2 = test_var2)))

  filter_obj <- filter("multi_discrete",
    variables = c("var1", "var2"), values = NA,
    dataset = "test_dataset", keep_na = TRUE
  )

  vdiffr::expect_doppelganger(
    "multi_discrete - standard call works",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data)
    }
  )

  vdiffr::expect_doppelganger(
    "multi_discrete - Extra args work",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data, axes = FALSE)
    }
  )

  vdiffr::expect_doppelganger(
    "multi_discrete - No data case works",
    fig = function() {
      cb_plot_filter_data(filter_obj, source, test_data_null)
    }
  )
})

# -- Domain integration tests ------------------------------------------------

test_that("autofilter populates domain from data", {
  source <- set_source(tblist(iris = iris)) |>
    autofilter(attach_as = "meta")
  filters <- source$available_filters
  species_filter <- purrr::detect(filters, ~ .x@id == "iris-Species")
  expect_false(is.null(species_filter@domain))
  expect_true(all(c("setosa", "versicolor", "virginica") %in% species_filter@domain))

  sl_filter <- purrr::detect(filters, ~ .x@id == "iris-SepalLength")
  expect_identical(sl_filter@domain, c(min(iris$Sepal.Length), max(iris$Sepal.Length)))
})

test_that("autofilter builds discrete_text domain as a comma-separated string", {
  # A column whose values are all unique becomes a discrete_text filter. Its
  # domain must be the single comma-separated string the discrete_text contract
  # expects, not the raw character vector, so cb_intersect_domain() round-trips
  # correctly. Regression: a vector domain made split_discrete_text() keep only
  # the first value, filtering the data down to a single row.
  df <- data.frame(
    spec_id = paste("spec", 1L:10L),
    x = 1L:10L,
    stringsAsFactors = FALSE
  )
  source <- set_source(tblist(t = df)) |>
    autofilter(attach_as = "meta")
  specid <- purrr::detect(source$available_filters, ~ .x@id == "t-specid")

  expect_s3_class(specid, "cohortBuilder::CbFilterDiscreteText")
  expect_length(specid@domain, 1L)
  expect_identical(specid@domain, paste(df$spec_id, collapse = ","))

  # value = NA means "no filtering": all rows must be kept, not just the first.
  coh <- cohort(source = source)
  coh$copy_step(filters = list(specid), run_flow = TRUE)
  expect_identical(nrow(coh$get_data(state = "post")$t), nrow(df))

  # Selecting several values keeps exactly those rows.
  coh$update_filter(
    coh$last_step_id(), specid@id,
    value = "spec 2, spec 7", run_flow = TRUE
  )
  kept <- coh$get_data(state = "post")$t
  expect_setequal(kept$spec_id, c("spec 2", "spec 7"))
})

test_that("autofilter inherits domain from describe()", {
  custom_domain <- c("setosa", "versicolor")
  source <- set_source(
    tblist(iris = iris),
    description = list(
      iris = list(
        Species = describe("species", domain = custom_domain)
      )
    )
  ) |> autofilter(attach_as = "meta")

  species_filter <- purrr::detect(source$available_filters, ~ .x@id == "iris-Species")
  expect_identical(species_filter@domain, custom_domain)
})

test_that("shape() returns datasets and filters lists", {
  custom_domain <- c("setosa", "versicolor")
  source <- set_source(
    tblist(iris = iris),
    description = list(
      iris = list(
        dataset_ = describe("iris data"),
        Species = describe("species", domain = custom_domain)
      )
    )
  ) |> autofilter(attach_as = "meta")

  result <- shape(source)
  expect_named(result, c("datasets", "filters"))
  expect_identical(result$datasets$iris, "iris data")
  expect_true("iris-Species" %in% names(result$filters))

  species <- result$filters[["iris-Species"]]
  expect_identical(species$dataset, "iris")
  expect_identical(species$type, "discrete")
  expect_identical(species$domain, custom_domain)
  expect_identical(species$description, "Species")
  expect_identical(
    species$variables,
    list(list(name = "Species", description = "species"))
  )
})

test_that("shape() filters is empty when no available_filters", {
  source <- set_source(
    tblist(iris = iris),
    description = list(
      iris = list(
        dataset_ = describe("iris data"),
        Species = describe("species")
      )
    )
  )

  result <- shape(source)
  expect_named(result, c("datasets", "filters"))
  expect_identical(result$datasets$iris, "iris data")
  expect_length(result$filters, 0L)
})

test_that("shape() domain falls back to meta stats when filter domain is unset", {
  source <- set_source(
    tblist(iris = iris),
    description = list(
      iris = list(dataset_ = describe("iris data"))
    )
  ) |> autofilter(attach_as = "meta")

  # Clear the declared domain to force the cache fallback path.
  source$available_filters <- purrr::map(source$available_filters, function(f) {
    f@domain <- NULL
    f
  })

  result <- shape(source)
  species <- result$filters[["iris-Species"]]
  expect_setequal(species$domain, as.character(collapse::funique(iris$Species)))
})

test_that("shape(source, field) still returns description text", {
  source <- set_source(
    tblist(iris = iris),
    description = list(
      iris = list(
        dataset_ = describe("iris data"),
        Species = describe("species text")
      )
    )
  ) |> autofilter(attach_as = "meta")

  expect_identical(shape(source, "iris"), "iris data")
  expect_identical(shape(source, "iris", "Species"), "species text")
})
