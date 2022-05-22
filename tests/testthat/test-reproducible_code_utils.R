test_that("parse_func_expr assigns last line to data_object variable", {
  test_fun_one <- function() {
    val <- a + 1
    val
  }
  expect_equal(
    parse_func_expr(test_fun_one),
    quote({
      val <- a + 1
      data_object <- val
    })
  )
})

test_that("parse_func_expr substitutes environment variables", {
  test_fun_one <- function(a = 2) {
    val <- a + 1
    val
  }
  expect_equal(
    parse_func_expr(test_fun_one),
    quote({
      val <- 2 + 1
      data_object <- val
    })
  )
})

test_that("combine_expressions merges multiple expressions into a single one", {
  test_fun_one <- function(data_object, b = 1) {
    data_object <- a + b
    data_object
  }
  test_fun_two <- function(data_object, d = 2) {
    data_object + d
  }

  expect_equal(
    combine_expressions(
      list(
        parse_func_expr(test_fun_one),
        parse_func_expr(test_fun_two)
      )
    ),
    quote({
      data_object <- a + 1
      data_object <- data_object
      data_object <- data_object + 2
    })
  )
})
