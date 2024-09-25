discrete_filter <- filter(
  type = "discrete", id = "age_filter", name = "Age", variable = "age", dataset = "patients", value = 50
)
patients_source <- set_source(
  tblist(patients = data.frame(id = 1:2, age = 50:51))
)
variable_filter <- discrete_filter(patients_source)

test_that("Calling filter with id returns function of source param, calling valid S3 method", {
  expect_equal(names(formals(discrete_filter)), "source")
  expect_equal(as.character(body(discrete_filter)[[2]][[2]]), "cb_filter.discrete")
  expect_true(is.function(discrete_filter))
})

test_that("Calling filter on source returns list with valid methods and parameters", {
  expect_true(is.list(variable_filter))
  expect_identical(
    c("id", "type", "name", "input_param", "filter_data", "get_stats", "plot_data", "get_params", "get_data", "get_defaults"),
    names(variable_filter)
  )
})

test_that("Filter methods operate correctly based on its definition", {
  expect_equal(variable_filter$filter_data(patients_source$dtconn)$patients$age, 50)
  expect_equal(variable_filter$get_stats(patients_source$dtconn)$choices, as.list(table(50:51)))
  expect_equal(class(variable_filter$plot_data(patients_source$dtconn)), c("matrix", "array"))
})

test_that("Discrete text filter works fine", {
  iris_source <- set_source(
    tblist(iris = iris)
  )
  spec_filter <- filter("discrete_text", id = "species", dataset = "iris", variable = "Species", value = "setosa,virginica")
  coh <- Cohort$new(
    iris_source,
    spec_filter
  )
  expect_equal(coh$get_data(1, state = "pre")$iris, iris)
  coh$run_flow()
  expect_setequal(unique(coh$get_data(1, state = "post")$iris$Species), c("setosa", "virginica"))
  expect_equal(
    coh$get_cache("1", "species", state = "post")$choices,
    "setosa,virginica"
  )
})

test_that("Multi discrete filter works fine", {
  md_data <- data.frame(col1 = c("A", "B", "A", "B", "A"), col2 = c("C", "C", "C", "D", "D"))
  md_source <- set_source(
    tblist(md_data = md_data)
  )
  md_filter <- cohortBuilder::filter(
    type = "multi_discrete", id = "mcols", name = "Multi Cols", dataset = "md_data",
    values = list(col1 = "A", col2 = "D"), variables = c("col1", "col2")
  )

  coh <- Cohort$new(
    md_source,
    md_filter
  )
  expect_equal(coh$get_data(1, state = "pre")$md_data, md_data)

  coh$run_flow()
  expect_setequal(unique(coh$get_data(1, state = "post")$md_data$col1), c("A"))
  expect_setequal(unique(coh$get_data(1, state = "post")$md_data$col2), c("D"))

  expect_equal(
    coh$get_cache("1", "mcols", state = "pre")$choices$col1,
    as.list(table(md_data$col1))
  )
  expect_equal(
    coh$get_cache("1", "mcols", state = "pre")$choices$col2,
    as.list(table(md_data$col2))
  )
  expect_equal(
    coh$get_cache("1", "mcols", state = "post")$choices$col1,
    as.list(table(c("A")))
  )
  expect_equal(
    coh$get_cache("1", "mcols", state = "post")$choices$col2,
    as.list(table(c("D")))
  )

})

test_that("Query discrete filter works fine", {
  md_data <- data.frame(col1 = c("A", "B", "A", "B", "A"), col2 = c("C", "C", "C", "D", "D"))
  md_source <- set_source(
    tblist(md_data = md_data)
  )
  md_filter <- cohortBuilder::filter(
    type = "query", id = "qcols", name = "Query Cols", dataset = "md_data",
    variables = c("col1", "col2"),
    value = queryBuilder::queryGroup(
      condition = "AND",
      queryBuilder::queryRule("col1", "equal", "A"),
      queryBuilder::queryRule("col2", "in", "D")
    )
  )

  coh <- Cohort$new(
    md_source,
    md_filter
  )
  expect_equal(coh$get_data(1, state = "pre")$md_data, md_data)

  coh$run_flow()
  expect_setequal(unique(coh$get_data(1, state = "post")$md_data$col1), c("A"))
  expect_setequal(unique(coh$get_data(1, state = "post")$md_data$col2), c("D"))

  expect_equal(
    coh$get_cache("1", "qcols", state = "pre")$specs$col1$values,
    unique(md_data$col1)
  )
  expect_equal(
    coh$get_cache("1", "qcols", state = "pre")$specs$col2$values,
    unique(md_data$col2)
  )
  expect_equal(
    coh$get_cache("1", "qcols", state = "post")$specs$col1$values,
    unique(c("A"))
  )
  expect_equal(
    coh$get_cache("1", "qcols", state = "post")$specs$col2$values,
    unique(c("D"))
  )

})
