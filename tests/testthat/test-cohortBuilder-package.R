# Test for force_import function
test_that("force_import loads necessary functions", {
  expect_silent(force_import())
})

# Test for %:::% operator
test_that("%:::% operator retrieves internal function", {
  expect_equal("jsonlite" %:::% "toJSON", jsonlite::toJSON)
})

# Test for %in% operator
test_that("%in% operator behaves as expected", {
  x <- c(1, 2, 3, 4, 5)
  table <- c(3, 4, 5, 6, 7)
  
  expect_equal(`%in%`(x, table), base::`%in%`(x, table))
  
  x <- c("apple", "banana", "cherry")
  table <- c("banana", "cherry", "date")
  
  expect_equal(`%in%`(x, table), base::`%in%`(x, table))
  expect_equal(x %in% table, base::`%in%`(x, table))
})
