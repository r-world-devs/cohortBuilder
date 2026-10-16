test_that("filter constructors preserve parent and child properties", {
  types <- c(
    "discrete",
    "discrete_text",
    "range",
    "date_range",
    "datetime_range",
    "multi_discrete",
    "query"
  )
  for (type in types) {
    args <- list(
      type = type,
      dataset = "patients",
      id = "age",
      name = "Age",
      active = FALSE,
      description = "Age filter",
      domain = c(1, 2)
    )
    if (type %in% c("multi_discrete", "query")) {
      args$variables <- "age"
    } else {
      args$variable <- "age"
    }
    if (type == "multi_discrete") {
      args$values <- list(age = 1)
    }
    x <- do.call(filter, args)
    expect_s7_class(x, CbFilter)
    expect_identical(x@type, type)
    expect_identical(x@id, "age")
    expect_identical(x@name, "Age")
    expect_identical(x@active, FALSE)
    expect_identical(x@description, "Age filter")
    expect_identical(x@domain, c(1, 2))
    expect_identical(x@dataset, "patients")
  }
})
