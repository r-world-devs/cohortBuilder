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
  expect_identical(discrete_filter@private$input_param, "value")
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
    coh$get_stats("1", "species", state = "post")$choices,
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
    coh$get_stats("1", "mcols", state = "pre")$choices$col1,
    as.list(table(md_data$col1))
  )
  expect_identical(
    coh$get_stats("1", "mcols", state = "pre")$choices$col2,
    as.list(table(md_data$col2))
  )
  expect_identical(
    coh$get_stats("1", "mcols", state = "post")$choices$col1,
    as.list(table(c("A")))
  )
  expect_identical(
    coh$get_stats("1", "mcols", state = "post")$choices$col2,
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
    coh$get_stats("1", "qcols", state = "pre")$specs$col1$values,
    collapse::funique(md_data$col1)
  )
  expect_identical(
    coh$get_stats("1", "qcols", state = "pre")$specs$col2$values,
    collapse::funique(md_data$col2)
  )
  expect_identical(
    coh$get_stats("1", "qcols", state = "post")$specs$col1$values,
    "A"
  )
  expect_identical(
    coh$get_stats("1", "qcols", state = "post")$specs$col2$values,
    "D"
  )

})

# -- Domain property tests ----------------------------------------------------

test_that("Filter created with domain stores it as S7 property", {
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    domain = c("setosa", "versicolor", "virginica")
  )
  expect_identical(f@domain, c("setosa", "versicolor", "virginica"))
})

test_that("Filter domain defaults to NULL when not provided", {
  f <- filter(type = "discrete", id = "sp", variable = "Species", dataset = "iris")
  expect_null(f@domain)
})

test_that("domain appears in get_filter_params output", {
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    domain = c("setosa", "versicolor")
  )
  params <- get_filter_params(f)
  expect_true("domain" %in% names(params))
  expect_identical(params$domain, c("setosa", "versicolor"))
})

test_that("domain works for all filter types", {
  f_range <- filter(
    type = "range", id = "sl", variable = "Sepal.Length", dataset = "iris",
    domain = c(4, 8)
  )
  expect_identical(f_range@domain, c(4, 8))

  f_date <- filter(
    type = "date_range", id = "d", variable = "date", dataset = "d",
    domain = as.Date(c("2020-01-01", "2025-12-31"))
  )
  expect_identical(f_date@domain, as.Date(c("2020-01-01", "2025-12-31")))

  f_md <- filter(
    type = "multi_discrete", id = "md", variables = c("a", "b"), dataset = "d",
    values = list(a = "x", b = "y"),
    domain = list(a = c("x", "y", "z"), b = c("y", "w"))
  )
  expect_identical(f_md@domain, list(a = c("x", "y", "z"), b = c("y", "w")))

  f_query <- filter(
    type = "query", id = "q", variables = "col1", dataset = "d",
    domain = NULL
  )
  expect_null(f_query@domain)
})

# -- .default_filter_id tests -------------------------------------------------

test_that(".default_filter_id generates correct ID for single variable", {
  expect_identical(.default_filter_id("iris", "Species"), "iris-Species")
})

test_that(".default_filter_id strips non-alphanumeric characters", {
  expect_identical(.default_filter_id("my_data", "Sepal.Length"), "mydata-SepalLength")
})

test_that(".default_filter_id handles suffix", {
  expect_identical(
    .default_filter_id("data", c("col1", "col2"), suffix = "md"),
    "data-col1-col2-md"
  )
})

test_that(".default_filter_id truncates to 3 vars + hash for >3 variables", {
  vars <- c("alpha", "beta", "gamma", "delta", "epsilon")
  result <- .default_filter_id("ds", vars, suffix = "md")
  parts <- strsplit(result, "-")[[1L]]
  expect_identical(parts[1L], "ds")
  expect_identical(parts[2L], "alpha")
  expect_identical(parts[3L], "beta")
  expect_identical(parts[4L], "gamma")
  expect_identical(nchar(parts[5L]), 4L)
  expect_identical(parts[6L], "md")
})

test_that(".default_filter_id hash is deterministic", {
  vars <- c("a", "b", "c", "d")
  id1 <- .default_filter_id("ds", vars, suffix = "q")
  id2 <- .default_filter_id("ds", vars, suffix = "q")
  expect_identical(id1, id2)
})

test_that(".default_filter_id hash is the same regardless of variable order", {
  id1 <- .default_filter_id("ds", c("a", "b", "c", "d"), suffix = "md")
  id2 <- .default_filter_id("ds", c("d", "c", "b", "a"), suffix = "md")
  # First 3 vars differ by input order, but the hash portion is identical
  hash1 <- strsplit(id1, "-")[[1L]][5L]
  hash2 <- strsplit(id2, "-")[[1L]][5L]
  expect_identical(hash1, hash2)
})

# -- deterministic ID integration tests ---------------------------------------

test_that("filter() generates deterministic ID when id is omitted", {
  f <- filter("discrete", variable = "Species", dataset = "iris")
  expect_identical(f@id, "iris-Species")
  # Single-variable filters default their name to the variable.
  expect_identical(f@name, "Species")
})

test_that("filter() respects explicit id", {
  f <- filter("discrete", id = "custom", variable = "Species", dataset = "iris")
  expect_identical(f@id, "custom")
})

test_that("filter() respects explicit name while generating id", {
  f <- filter("discrete", name = "My Filter", variable = "Species", dataset = "iris")
  expect_identical(f@id, "iris-Species")
  expect_identical(f@name, "My Filter")
})

test_that("filter() name defaults to the variable even with explicit id", {
  f <- filter("discrete", id = "my_id", variable = "Species", dataset = "iris")
  expect_identical(f@name, "Species")
})

test_that("range filter gets deterministic ID", {
  f <- filter("range", variable = "Sepal.Width", dataset = "iris")
  expect_identical(f@id, "iris-SepalWidth")
})

test_that("multi_discrete filter gets ID with md suffix", {
  f <- filter("multi_discrete", variables = c("col1", "col2"), values = NA, dataset = "data")
  expect_identical(f@id, "data-col1-col2-md")
})

test_that("query filter gets ID with q suffix", {
  f <- filter("query", variables = c("col1", "col2"), dataset = "data")
  expect_identical(f@id, "data-col1-col2-q")
})

# -- default filter names ------------------------------------------------------

test_that("single-variable filters default name to the variable", {
  expect_identical(
    filter("range", variable = "Sepal.Width", dataset = "iris")@name,
    "Sepal.Width"
  )
  expect_identical(
    filter("discrete_text", variable = "Species", dataset = "iris")@name,
    "Species"
  )
})

test_that("multi-variable filters summarise variables and type in default name", {
  expect_identical(
    filter("multi_discrete", variables = "col1", values = NA, dataset = "d")@name,
    "col1 multi_discrete"
  )
  expect_identical(
    filter("multi_discrete", variables = c("col1", "col2"), values = NA,
           dataset = "d")@name,
    "col1 col2 multi_discrete"
  )
  expect_identical(
    filter("multi_discrete", variables = c("col1", "col2", "col3"), values = NA,
           dataset = "d")@name,
    "col1 col2 + 1 vars multi_discrete"
  )
  expect_identical(
    filter("query", variables = c("a", "b", "c", "d", "e"), dataset = "d")@name,
    "a b + 3 vars query"
  )
})

test_that("explicit name overrides default for multi-variable filters", {
  f <- filter("multi_discrete", name = "Custom", variables = c("a", "b", "c"),
              values = NA, dataset = "d")
  expect_identical(f@name, "Custom")
})

test_that(".default_filter_name summarises by variable count", {
  expect_identical(.default_filter_name("a", "query"), "a query")
  expect_identical(.default_filter_name(c("a", "b"), "query"), "a b query")
  expect_identical(
    .default_filter_name(c("a", "b", "c"), "query"),
    "a b + 1 vars query"
  )
  expect_identical(
    .default_filter_name(c("a", "b", "c", "d"), "multi_discrete"),
    "a b + 2 vars multi_discrete"
  )
})

# -- cb_intersect_domain tests ------------------------------------------------

test_that("cb_intersect_domain returns NA when no domain and value is NA", {
  f <- filter(type = "discrete", id = "x", variable = "a", dataset = "d")
  expect_identical(cb_intersect_domain(f), NA)
})

test_that("cb_intersect_domain returns value when no domain", {
  f <- filter(type = "discrete", id = "x", variable = "a", dataset = "d", value = c("a", "b"))
  expect_identical(cb_intersect_domain(f), c("a", "b"))
})

test_that("cb_intersect_domain returns domain when value is NA", {
  f <- filter(
    type = "discrete", id = "x", variable = "a", dataset = "d",
    domain = c("a", "b", "c")
  )
  expect_identical(cb_intersect_domain(f), c("a", "b", "c"))
})

test_that("cb_intersect_domain intersects value with domain for discrete", {
  f <- filter(
    type = "discrete", id = "x", variable = "a", dataset = "d",
    value = c("a", "b", "z"), domain = c("a", "b", "c")
  )
  expect_warning(result <- cb_intersect_domain(f), "trimmed to domain")
  expect_identical(result, c("a", "b"))
})

test_that("cb_intersect_domain does not warn when value already within domain", {
  f <- filter(
    type = "discrete", id = "x", variable = "a", dataset = "d",
    value = c("a", "b"), domain = c("a", "b", "c")
  )
  expect_silent(result <- cb_intersect_domain(f))
  expect_identical(result, c("a", "b"))
})

test_that("cb_intersect_domain works for range types", {
  f <- filter(
    type = "range", id = "x", variable = "a", dataset = "d",
    range = c(1, 100), domain = c(10, 50)
  )
  expect_warning(result <- cb_intersect_domain(f), "trimmed to domain")
  expect_identical(result, c(10, 50))
})

test_that("cb_intersect_domain returns domain when range is NA", {
  f <- filter(
    type = "range", id = "x", variable = "a", dataset = "d",
    domain = c(10, 50)
  )
  expect_identical(cb_intersect_domain(f), c(10, 50))
})

test_that("cb_intersect_domain works for multi_discrete", {
  f <- filter(
    type = "multi_discrete", id = "x", variables = c("a", "b"), dataset = "d",
    values = list(a = c("x", "z"), b = c("y")),
    domain = list(a = c("x", "y"), b = c("y", "w"))
  )
  expect_warning(result <- cb_intersect_domain(f), "trimmed to domain")
  expect_identical(result, list(a = "x", b = "y"))
})

test_that("cb_intersect_domain returns value as-is for query filters", {
  qval <- queryBuilder::queryGroup(
    condition = "AND",
    queryBuilder::queryRule("col1", "equal", "A")
  )
  f <- filter(type = "query", id = "q", variables = "col1", dataset = "d", value = qval)
  expect_identical(cb_intersect_domain(f), qval)
})

test_that("cb_intersect_domain default method returns raw value for custom filter", {
  CustomFilter <- S7::new_class("CustomFilter",
    parent = CbFilter,
    properties = list(
      dataset = S7::class_character,
      variable = S7::class_character,
      value = S7::class_any
    ),
    constructor = function(id = .gen_id(), name = id, variable, value = NA,
                           dataset, domain = NULL, ...) {
      S7::new_object(S7::S7_object(),
        type = "custom", id = id, name = name,
        variable = variable, value = value, dataset = dataset,
        active = TRUE, description = NULL, domain = domain,
        extra = list(...), private = list(input_param = "value")
      )
    }
  )
  f <- CustomFilter(id = "c1", variable = "x", value = c("a", "b"), dataset = "d")
  expect_identical(cb_intersect_domain(f), c("a", "b"))
})

# -- Domain filtering end-to-end tests ----------------------------------------

test_that("Discrete filter with domain constrains results", {
  iris_source <- set_source(tblist(iris = iris))
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    value = c("setosa", "versicolor", "virginica"),
    domain = c("setosa", "versicolor")
  )
  coh <- Cohort$new(iris_source, f)
  suppressWarnings(coh$run_flow())
  result <- coh$get_data(1L, state = "post")$iris
  expect_true(all(result$Species %in% c("setosa", "versicolor")))
})

test_that("Range filter with domain constrains results", {
  iris_source <- set_source(tblist(iris = iris))
  f <- filter(
    type = "range", id = "sl", variable = "Sepal.Length", dataset = "iris",
    range = c(1, 10), domain = c(5, 6)
  )
  coh <- Cohort$new(iris_source, f)
  suppressWarnings(coh$run_flow())
  result <- coh$get_data(1L, state = "post")$iris
  expect_true(all(result$Sepal.Length >= 5 & result$Sepal.Length <= 6))
})

test_that("update_filter can change domain", {
  iris_source <- set_source(tblist(iris = iris))
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    domain = c("setosa", "versicolor", "virginica")
  )
  coh <- Cohort$new(iris_source, f)
  coh$update_filter("1", "sp", domain = c("setosa"))
  expect_identical(coh$get_filter("1", "sp")@domain, "setosa")
})

test_that("get_state/restore round-trips domain", {
  iris_source <- set_source(tblist(iris = iris))
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    domain = c("setosa", "versicolor")
  )
  coh <- Cohort$new(iris_source, f)
  state <- coh$get_state()

  coh2 <- Cohort$new(iris_source, f)
  coh2$restore(state)
  expect_identical(coh2$get_filter("1", "sp")@domain, c("setosa", "versicolor"))
})

# -- cb_filter_to_expr domain tests -------------------------------------------

test_that("cb_filter_to_expr uses domain-intersected value for discrete filter", {
  iris_source <- set_source(tblist(iris = iris))
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    value = c("setosa", "versicolor", "virginica"),
    domain = c("setosa", "versicolor")
  )
  expr <- suppressWarnings(cb_filter_to_expr(f, iris_source))
  # Evaluate the generated code to verify it uses the intersected value
  data_object <- iris_source$dtconn
  eval(expr)
  expect_true(all(data_object$iris$Species %in% c("setosa", "versicolor")))
})

test_that("cb_filter_to_expr uses domain as value when range is NA", {
  iris_source <- set_source(tblist(iris = iris))
  f <- filter(
    type = "range", id = "sl", variable = "Sepal.Length", dataset = "iris",
    domain = c(5, 6)
  )
  expr <- cb_filter_to_expr(f, iris_source)
  data_object <- iris_source$dtconn
  eval(expr)
  expect_true(all(data_object$iris$Sepal.Length >= 5 & data_object$iris$Sepal.Length <= 6))
})

# -- discrete_text comma-separated value / domain -----------------------------

# The discrete_text filter represents its value, choices and domain as a single
# comma-separated string (e.g. "a, b, c"), unlike the plain discrete filter
# which uses character vectors and named-count choices. These tests pin that the
# domain handling stays in the comma-separated string form and round-trips.

test_that("split_discrete_text trims whitespace around every value", {
  expect_identical(split_discrete_text("a, b, c"), c("a", "b", "c"))
  expect_identical(split_discrete_text("a,b,c"), c("a", "b", "c"))
  expect_identical(split_discrete_text("  a ,  b  "), c("a", "b"))
  expect_identical(split_discrete_text(NA), character(0L))
  expect_identical(split_discrete_text(""), character(0L))
  expect_identical(split_discrete_text(NULL), character(0L))
})

test_that("join_discrete_text produces a comma-separated string", {
  expect_identical(join_discrete_text(c("a", "b", "c")), "a,b,c")
  expect_identical(join_discrete_text(character(0L)), "")
})

test_that("cb_intersect_domain for discrete_text returns a comma string", {
  f <- filter(
    type = "discrete_text", id = "g", variable = "g", dataset = "d",
    value = "a, b", domain = "a,b,c,d"
  )
  expect_identical(cb_intersect_domain(f), "a,b")
})

test_that("cb_intersect_domain for discrete_text returns domain when value is NA", {
  f <- filter(
    type = "discrete_text", id = "g", variable = "g", dataset = "d",
    domain = "a,b,c"
  )
  expect_identical(cb_intersect_domain(f), "a,b,c")
})

test_that("cb_intersect_domain for discrete_text trims value to domain (with warning)", {
  f <- filter(
    type = "discrete_text", id = "g", variable = "g", dataset = "d",
    value = "a, z", domain = "a,b,c"
  )
  expect_warning(result <- cb_intersect_domain(f), "trimmed to domain")
  expect_identical(result, "a")
})

test_that("cb_intersect_domain for discrete_text is silent when value within domain", {
  f <- filter(
    type = "discrete_text", id = "g", variable = "g", dataset = "d",
    value = "a, b", domain = "a,b,c"
  )
  expect_silent(result <- cb_intersect_domain(f))
  expect_identical(result, "a,b")
})

test_that("cb_domain_from_stats for discrete_text reads comma-separated choices", {
  f <- filter(type = "discrete_text", id = "g", variable = "g", dataset = "d")
  expect_identical(
    cb_domain_from_stats(f, list(choices = "a,b,c,d")), "a,b,c,d"
  )
  expect_null(cb_domain_from_stats(f, list(choices = NULL)))
})

test_that("cb_domain_from_data for discrete_text returns a comma-separated string", {
  df <- data.frame(g = c("a", "b", "c", "d", "a"), stringsAsFactors = FALSE)
  src <- set_source(tblist(d = df))
  f <- filter(type = "discrete_text", id = "g", variable = "g", dataset = "d")
  expect_identical(cb_domain_from_data(f, src, list(d = df)), "a,b,c,d")
})

test_that("cb_intersect_domain_values for discrete_text intersects two comma strings", {
  f <- filter(type = "discrete_text", id = "g", variable = "g", dataset = "d")
  expect_identical(
    cb_intersect_domain_values(f, "a,b,c", "b,c,d"), "b,c"
  )
  expect_identical(cb_intersect_domain_values(f, NULL, "a,b"), "a,b")
  expect_identical(cb_intersect_domain_values(f, "a,b", NULL), "a,b")
})

test_that("discrete_text filter with domain constrains results", {
  df <- data.frame(g = c("a", "b", "c", "d", "a", "b"), stringsAsFactors = FALSE)
  src <- set_source(tblist(d = df))
  f <- filter(
    type = "discrete_text", id = "g", variable = "g", dataset = "d",
    value = "a, b, c", domain = "a,b"
  )
  coh <- Cohort$new(src, f)
  suppressWarnings(coh$run_flow())
  result <- coh$get_data(1L, state = "post")$d
  expect_true(all(result$g %in% c("a", "b")))
  expect_false("c" %in% result$g)
})

test_that("discrete_text filter splits 3+ space-separated values correctly", {
  # Regression: sub(" ", "", x) only stripped the FIRST space, so the third
  # value (" c") kept a leading space and matched nothing.
  df <- data.frame(g = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  src <- set_source(tblist(d = df))
  f <- filter(
    type = "discrete_text", id = "g", variable = "g", dataset = "d",
    value = "a, b, c"
  )
  coh <- Cohort$new(src, f)
  coh$run_flow()
  result <- coh$get_data(1L, state = "post")$d
  expect_setequal(unique(result$g), c("a", "b", "c"))
})

test_that("stats-mode propagation narrows a discrete_text domain", {
  df <- data.frame(g = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  src <- set_source(tblist(d = df))
  coh <- Cohort$new(
    src,
    step(filter(
      type = "discrete_text", id = "g", variable = "g", dataset = "d",
      value = "a, b, c"
    )),
    step(filter(
      type = "discrete_text", id = "g", variable = "g", dataset = "d",
      domain = "a,b,c,d"
    )),
    propagate_domains = "stats"
  )
  coh$run_flow()
  # Step 2's domain should be narrowed to the values surviving step 1 (a, b, c).
  expect_identical(
    sort(split_discrete_text(coh$get_filter("2", "g")@domain)),
    c("a", "b", "c")
  )
})

# -- filter_domain() ----------------------------------------------------------

test_that("filter_domain() returns the declared domain", {
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    domain = c("setosa", "versicolor")
  )
  expect_identical(filter_domain(f), c("setosa", "versicolor"))
})

test_that("filter_domain() returns NULL when no domain is set", {
  f <- filter(type = "discrete", id = "sp", variable = "Species", dataset = "iris")
  expect_null(filter_domain(f))
})

test_that("filter_domain() works for range filters", {
  f <- filter(
    type = "range", id = "sl", variable = "Sepal.Length", dataset = "iris",
    domain = c(5, 6)
  )
  expect_identical(filter_domain(f), c(5, 6))
})

# -- filter_effective_value() -------------------------------------------------

test_that("filter_effective_value() intersects discrete value with domain", {
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    value = c("setosa", "versicolor", "virginica"),
    domain = c("setosa", "versicolor")
  )
  expect_setequal(
    suppressWarnings(filter_effective_value(f)),
    c("setosa", "versicolor")
  )
})

test_that("filter_effective_value() returns domain when value is NA", {
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    domain = c("setosa", "versicolor")
  )
  expect_identical(filter_effective_value(f), c("setosa", "versicolor"))
})

test_that("filter_effective_value() returns range domain when range is NA", {
  f <- filter(
    type = "range", id = "sl", variable = "Sepal.Length", dataset = "iris",
    domain = c(5, 6)
  )
  expect_identical(filter_effective_value(f), c(5, 6))
})

test_that("filter_effective_value() returns raw value when no domain", {
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    value = c("setosa", "versicolor")
  )
  expect_identical(filter_effective_value(f), c("setosa", "versicolor"))
})
