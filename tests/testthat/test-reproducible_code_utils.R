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

test_that("cb_filter_to_expr generates expression for S7 filters", {
  discrete_filter <- filter(
    type = "discrete", id = "species_filter", name = "Species",
    variable = "Species", dataset = "iris", value = c("setosa", "virginica")
  )
  iris_source <- set_source(tblist(iris = iris))

  result <- cb_filter_to_expr(discrete_filter, iris_source)

  expect_type(result, "language")
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

test_that("cb_filter_to_expr returns NULL for no-op filters", {
  noop_filter <- filter(
    type = "discrete", id = "noop", name = "Noop",
    variable = "Species", dataset = "iris", value = NA, keep_na = TRUE
  )
  iris_source <- set_source(tblist(iris = iris))

  result <- cb_filter_to_expr(noop_filter, iris_source)
  expect_null(result)
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
