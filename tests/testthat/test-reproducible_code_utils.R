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

# -- type_expr -----------------------------------------------------------------

test_that("type_expr uses action column without conflicting with filter type", {
  te <- type_expr(action = "filtering", step = "1",
                  expr = quote(x + 1L), type = "discrete", dataset = "iris")
  df <- te[[1L]]
  expect_true("action" %in% names(df))
  expect_true("type" %in% names(df))
  expect_identical(df$action, "filtering")
  expect_identical(df$type[[1L]], "discrete")
})

# -- exclude_first_pipe --------------------------------------------------------

test_that("exclude_first_pipe handles bare symbols without error", {
  result <- exclude_first_pipe(quote(x), quote(y))
  expect_identical(result, quote(x))
})

test_that("exclude_first_pipe removes first arg from function call", {
  # dplyr::filter(data, condition) -> dplyr::filter(condition)
  expr <- quote(dplyr::filter(data, x > 1L))
  result <- exclude_first_pipe(expr, quote(data))
  expect_identical(result, quote(dplyr::filter(x > 1L)))
})

test_that("exclude_first_pipe leaves call unchanged when first arg doesn't match", {
  expr <- quote(dplyr::filter(other, x > 1L))
  result <- exclude_first_pipe(expr, quote(data))
  expect_identical(result, expr)
})

# -- exclude_reassignment -----------------------------------------------------

test_that("exclude_reassignment strips assignment from braced function call expr", {
  # { data[["x"]] <- dplyr::filter(data[["x"]], cond) }
  expr <- quote({
    data[["x"]] <- dplyr::filter(data[["x"]], cond)
  })
  result <- exclude_reassignment(expr, along_with = "left")
  # Should strip the <- leaving dplyr::filter(data[["x"]], cond)
  expect_identical(result[[2L]][[1L]], quote(dplyr::filter))
  expect_length(result[[2L]], 3L) # dplyr::filter, data[["x"]], cond
})

test_that("exclude_reassignment with both removes data ref from function call", {
  expr <- quote({
    data[["x"]] <- dplyr::filter(data[["x"]], cond)
  })
  result <- exclude_reassignment(expr, along_with = "both")
  # Should strip <- AND remove data[["x"]] from filter args
  expect_identical(result[[2L]], quote(dplyr::filter(cond)))
})

# -- pipe_reassignment --------------------------------------------------------

test_that("pipe_reassignment inserts lhs as first argument of rhs", {
  lhs <- quote(dplyr::filter(data, cond1))
  rhs <- quote(dplyr::filter(cond2))
  result <- pipe_reassignment(lhs, rhs)
  # Result should be dplyr::filter(dplyr::filter(data, cond1), cond2)
  expect_identical(result[[1L]], quote(dplyr::filter))
  expect_identical(result[[2L]], lhs)
  expect_identical(result[[3L]], quote(cond2))
})

test_that("pipe_reassignment produces evaluable expression (no call to |>)", {
  lhs <- quote(1L + 2L)
  rhs <- quote(sum(3L))
  result <- pipe_reassignment(lhs, rhs)
  # Should be sum(1L + 2L, 3L), not `|>`(1L + 2L, sum(3L))
  expect_identical(result[[1L]], quote(sum))
  val <- eval(result)
  expect_identical(val, 6L)
})

# -- pipe_filtering ------------------------------------------------------------

test_that("pipe_filtering combines function-call style filter expressions", {
  # Simulates two dplyr::filter calls on the same dataset (non-pipe style)
  e1 <- quote({
    data[["iris"]] <- dplyr::filter(data[["iris"]], Species == "setosa")
  })
  e2 <- quote({
    data[["iris"]] <- dplyr::filter(data[["iris"]], Sepal.Length > 5L)
  })
  result <- pipe_filtering(list(e1, e2))
  expect_length(result, 1L)

  combined <- result[[1L]]
  # The result should be evaluable
  data <- list(iris = iris)
  eval(combined)
  expect_true(all(data[["iris"]]$Species == "setosa"))
  expect_true(all(data[["iris"]]$Sepal.Length > 5L))
})

test_that("pipe_filtering returns single expression unchanged", {
  e <- quote({
    data[["iris"]] <- dplyr::filter(data[["iris"]], Species == "setosa")
  })
  result <- pipe_filtering(list(e))
  expect_identical(result, list(e))
})

# -- pipe_all_filters (integration) -------------------------------------------

test_that("pipe_all_filters combines filtering rows into single piped expression", {
  iris_source <- set_source(tblist(iris = iris))
  f1 <- filter(
    type = "discrete", id = "sp", name = "Species",
    variable = "Species", dataset = "iris",
    value = c("setosa", "virginica")
  )
  f2 <- filter(
    type = "range", id = "sl", name = "Sepal.Length",
    variable = "Sepal.Length", dataset = "iris",
    range = c(5L, 7L)
  )
  te1 <- type_expr(
    action = "filtering", step = "1",
    expr = cb_filter_to_expr(f1, iris_source),
    !!!get_filter_params(f1)
  )
  te2 <- type_expr(
    action = "filtering", step = "1",
    expr = cb_filter_to_expr(f2, iris_source),
    !!!get_filter_params(f2)
  )
  expr_df <- dplyr::bind_rows(te1[[1L]], te2[[1L]])

  result <- pipe_all_filters(expr_df)
  expect_true("action" %in% names(result))
  expect_true("expr" %in% names(result))
  # Should combine two filtering rows into one
  filtering_rows <- result[result$action == "filtering", ]
  expect_identical(nrow(filtering_rows), 1L)

  # The combined expression should be evaluable and produce correct results
  source <- list(dtconn = tblist(iris = iris))
  data_object <- source$dtconn
  eval(filtering_rows$expr[[1L]])
  expect_true(all(data_object[["iris"]]$Species %in% c("setosa", "virginica", NA)))
  expect_true(all(
    (data_object[["iris"]]$Sepal.Length <= 7L & data_object[["iris"]]$Sepal.Length >= 5L) |
      is.na(data_object[["iris"]]$Sepal.Length)
  ))
})

# -- assign_expr ---------------------------------------------------------------

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
