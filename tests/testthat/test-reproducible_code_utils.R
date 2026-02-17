test_that("parse_func_expr assigns last line to data_object variable", {
  test_fun_one <- function() {
    val <- a + 1L
    val
  }
  expect_identical(
    parse_func_expr(test_fun_one),
    quote({
      val <- a + 1L
      data_object <- val
    })
  )
})

test_that("parse_func_expr substitutes environment variables", {
  test_fun_one <- function(a = 2L) {
    val <- a + 1L
    val
  }
  expect_identical(
    parse_func_expr(test_fun_one),
    quote({
      val <- 2L + 1L
      data_object <- val
    })
  )
})

test_that("parse_func_expr returns an empty expression when func is NULL", {
  expect_identical(parse_func_expr(NULL),
                   quote({}))
})

test_that("combine_expressions merges multiple expressions into a single one", {
  test_fun_one <- function(data_object, b = 1L) {
    data_object <- a + b
    data_object
  }
  test_fun_two <- function(data_object, d = 2L) {
    data_object + d
  }

  expect_identical(
    combine_expressions(
      list(
        parse_func_expr(test_fun_one),
        parse_func_expr(test_fun_two)
      )
    ),
    quote({
      data_object <- a + 1L
      data_object <- data_object
      data_object <- data_object + 2L
    })
  )
})

test_that("pair_seq handles empty input gracefully", {
  # Given an empty input, expect empty integer vector
  result <- pair_seq(integer(0L))
  expect_type(result, "integer")
  expect_identical(result, integer(0L))
  expect_length(result, 0L)
})

test_that("pair_seq requires an even number of indexes", {
  # If odd length input is provided, function should fail
  expect_error(pair_seq(c(1L, 2L, 3L)), regexp = "The lenght of idxs is not even number")
})

test_that("pair_seq always returns a strictly increasing sequence of integers", {
  # Check that output is sorted and has no duplicates for a known even-length input
  input <- c(3L, 1L, 7L, 5L)   # Unsorted input
  result <- pair_seq(input)

  # Expect numeric output
  expect_type(result, "integer")

  # Expect output is in strictly ascending order
  expect_true(all(diff(result) > 0L))

})

test_that("parse_func_expr returns an empty expression when func is NULL", {
  expect_identical(parse_func_expr(NULL),
                   quote({}))
})

test_that("func_to_expr returns an empty expression when func is NULL", {
  expect_identical(func_to_expr(NULL, "test"),
                   quote({}))
})

test_that("func_to_expr returns a language object that includes the specified function name", {
  test_fun_one <- function() {
    val <- a + 1L
    val
  }
  name <- "simple_func_name"
  result <- func_to_expr(test_fun_one, name)

  expect_type(result, "language")
  expect_identical(as.character(result[2L]), name)
})

test_that("parse_filter_expr works fine", {
  discrete_iris_one <- filter(
    type = "discrete", id = "species_filter", name = "Species",
    variable = "Species", dataset = "iris", value = c("setosa", "virginica")
  )

  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one)
  )

  result <- parse_filter_expr(coh$get_filter(1L, 1L))

  expect_type(result, "language")
})

test_that("method_to_expr works fine", {
  expect_null(method_to_expr("not_existing_name", "not_existing_namespace"))
})

test_that("method_to_expr return function works fine", {
  name <- ".pre_filtering"
  namespace <- "tblist"

  result <- method_to_expr(name, namespace)

  expect_type(result, "language")
  expect_identical(formals(eval(result)), formals(paste0(name, ".", namespace)))
})

test_that("assign_expr works fine", {
  body_of_function <- quote(function(a = 1L, b = 1L) {
    a + b
  })
  result <- assign_expr(quote(function_name), body_of_function)

  eval_result <- eval(result)
  eval_body <- eval(body_of_function)

  expect_type(result, "language")
  expect_identical(body(eval_result), body(eval_body))
  expect_identical(formals(eval_result), formals(eval_body))
})
