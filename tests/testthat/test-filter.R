discrete_filter <- filter(
  type = "discrete", id = "age_filter", name = "Age", variable = "age", dataset = "patients", value = 50L
)
patients_source <- set_source(
  tblist(patients = data.frame(id = 1L:2L, age = 50L:51L))
)

test_that("Calling filter returns S7 object with correct class", {
  expect_true(S7::S7_inherits(discrete_filter, CbFilterDiscrete))
  expect_true(S7::S7_inherits(discrete_filter, CbFilter))
  expect_identical(discrete_filter@type, "discrete")
  expect_identical(discrete_filter@id, "age_filter")
  expect_identical(discrete_filter@name, "Age")
  expect_identical(discrete_filter@variable, "age")
  expect_identical(discrete_filter@dataset, "patients")
  expect_identical(discrete_filter@value, 50L)
  expect_identical(discrete_filter@input_param, "value")
})

test_that("Filter properties can be accessed via get_filter_params", {
  params <- get_filter_params(discrete_filter)
  expect_type(params, "list")
  expect_true(all(c("type", "id", "name", "variable", "value", "dataset", "active", "keep_na") %in% names(params)))
})

test_that("Filter S7 generics dispatch correctly on tblist source", {
  data_object <- patients_source$dtconn
  filtered <- cb_filter_data(discrete_filter, patients_source, data_object)
  expect_identical(filtered$patients$age, 50L)
  stats <- cb_get_filter_stats(discrete_filter, patients_source, data_object)
  expect_identical(stats$choices, as.list(table(50L:51L)))
})

test_that("Discrete text filter works fine", {
  iris_source <- set_source(
    tblist(iris = iris)
  )
  spec_filter <- filter("discrete_text", id = "species", dataset = "iris",
                        variable = "Species", value = "setosa,virginica")
  coh <- Cohort$new(
    iris_source,
    spec_filter
  )
  expect_identical(coh$get_data(1L, state = "pre")$iris, iris)
  coh$run_flow()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$iris$Species), c("setosa", "virginica"))
  expect_identical(
    coh$get_cache("1", "species", state = "post")$choices,
    "setosa,virginica"
  )
})

test_that("Multi discrete filter works fine", {
  md_data <- data.frame(col1 = c("A", "B", "A", "B", "A"), col2 = c("C", "C", "C", "D", "D"), stringsAsFactors = FALSE)
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
  expect_identical(coh$get_data(1L, state = "pre")$md_data, md_data)

  coh$run_flow()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$md_data$col1), c("A"))
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$md_data$col2), c("D"))

  expect_identical(
    coh$get_cache("1", "mcols", state = "pre")$choices$col1,
    as.list(table(md_data$col1))
  )
  expect_identical(
    coh$get_cache("1", "mcols", state = "pre")$choices$col2,
    as.list(table(md_data$col2))
  )
  expect_identical(
    coh$get_cache("1", "mcols", state = "post")$choices$col1,
    as.list(table(c("A")))
  )
  expect_identical(
    coh$get_cache("1", "mcols", state = "post")$choices$col2,
    as.list(table(c("D")))
  )

})

test_that("Query discrete filter works fine", {
  md_data <- data.frame(col1 = c("A", "B", "A", "B", "A"), col2 = c("C", "C", "C", "D", "D"), stringsAsFactors = FALSE)
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
  expect_identical(coh$get_data(1L, state = "pre")$md_data, md_data)

  coh$run_flow()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$md_data$col1), c("A"))
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$md_data$col2), c("D"))

  expect_identical(
    coh$get_cache("1", "qcols", state = "pre")$specs$col1$values,
    collapse::funique(md_data$col1)
  )
  expect_identical(
    coh$get_cache("1", "qcols", state = "pre")$specs$col2$values,
    collapse::funique(md_data$col2)
  )
  expect_identical(
    coh$get_cache("1", "qcols", state = "post")$specs$col1$values,
    "A"
  )
  expect_identical(
    coh$get_cache("1", "qcols", state = "post")$specs$col2$values,
    "D"
  )

})
