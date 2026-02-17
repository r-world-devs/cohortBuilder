# Test for force_import function
test_that("force_import loads necessary functions", {
  expect_silent(force_import())
})

# Test for %:::% operator
test_that("%:::% operator retrieves internal function", {
  expect_identical("jsonlite" %:::% "toJSON", jsonlite::toJSON)
})

# Test for %in% operator
test_that("%in% operator behaves as expected", {
  x <- c(1L, 2L, 3L, 4L, 5L)
  table <- c(3L, 4L, 5L, 6L, 7L)

  expect_identical(`%in%`(x, table), base::`%in%`(x, table))

  x <- c("apple", "banana", "cherry")
  table <- c("banana", "cherry", "date")

  expect_identical(`%in%`(x, table), base::`%in%`(x, table))
  expect_identical(x %in% table, base::`%in%`(x, table))
})
