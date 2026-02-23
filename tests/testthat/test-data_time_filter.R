test_that("calculate_datetime_step selects appropriate step", {
  # Define min and max dates for different ranges

  # 1. Test for a small range in minutes (expecting "mins" step)
  min_date <- as.POSIXct("2023-01-01 12:00:00")
  max_date <- as.POSIXct("2023-01-01 15:00:00")  # 3 hours later
  expect_identical(calculate_datetime_step(min_date, max_date) %>% unname(), 60L)

  # 2. Test for a range in hours (expecting "hours" step)
  min_date <- as.POSIXct("2023-01-01 12:00:00")
  max_date <- as.POSIXct("2023-01-02 12:00:00")  # 1 day later
  expect_identical(calculate_datetime_step(min_date, max_date) %>% unname(), 3600L)

  # 3. Test for a range in days (expecting "days" step)
  min_date <- as.POSIXct("2023-01-01 12:00:00")
  max_date <- as.POSIXct("2023-03-01 12:00:00")  # 7 month later
  expect_identical(calculate_datetime_step(min_date, max_date) %>% unname(), 86400L)

  # 4. Test for a range in weeks (expecting "weeks" step)
  min_date <- as.POSIXct("2023-01-01 12:00:00")
  max_date <- as.POSIXct("2023-08-01 12:00:00")  # 10 months later
  expect_identical(calculate_datetime_step(min_date, max_date) %>% unname(), 604800L)

  # 5. Test for a range in months (expecting "months" step)
  min_date <- as.POSIXct("2023-01-01 12:00:00")
  max_date <- as.POSIXct("2033-01-01 12:00:00")  # 10 year later
  expect_identical(calculate_datetime_step(min_date, max_date) %>% unname(), 2592000L)

  # 6. Test for a range in years (expecting "years" step)
  min_date <- as.POSIXct("1900-01-01 12:00:00")
  max_date <- as.POSIXct("2000-01-01 12:00:00")  # 100 years later
  expect_identical(calculate_datetime_step(min_date, max_date) %>% unname(), 31104000L)
})

test_that("cb_filter.datetime_range.tblist applies date time range filter correctly", {
  # Test data
  data <- data.frame(
    date_var = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-02 12:00:00", "2023-01-03 00:00:00", NA)),
    value = 1L:4L
  )
  data_object <- list(dataset_name = data)

  # Helper function to create a filter and apply it to data
  apply_filter <- function(range, keep_na = TRUE) {
    filter <- cb_filter.datetime_range.tblist(
      source = data_object, variable = "date_var", range = range,
      dataset = "dataset_name", keep_na = keep_na
    )
    filter$filter_data(data_object)
  }

  # 1. Test filtering within a specific range including NAs
  result <- apply_filter(range = c(as.POSIXct("2023-01-02"), as.POSIXct("2023-01-03")), keep_na = TRUE)
  expect_identical(nrow(result$dataset_name), 3L)  # Keeps rows in range + NA

  # 2. Test filtering within a specific range excluding NAs
  result <- apply_filter(range = c(as.POSIXct("2023-01-02"), as.POSIXct("2023-01-03")), keep_na = FALSE)
  expect_identical(nrow(result$dataset_name), 2L)  # Only rows in range, no NA

  # 3. Test filtering with NA range to keep all data including NAs
  result <- apply_filter(range = NA, keep_na = TRUE)
  expect_identical(nrow(result$dataset_name), 4L)  # All rows including NA

  # 4. Test filtering with NA range to keep only non-NA data
  result <- apply_filter(range = NA, keep_na = FALSE)
  expect_identical(nrow(result$dataset_name), 3L)  # All rows excluding NA

  # 5. Test filtering with c(Inf, -Inf) range to keep only NA values
  result <- apply_filter(range = c(Inf, -Inf), keep_na = TRUE)
  expect_identical(nrow(result$dataset_name), 1L)  # All rows including NA

  # 6. Test end boundary only (Inf) to include all data with NA
  result <- apply_filter(range = c(as.POSIXct("2023-01-01"), Inf), keep_na = TRUE)
  expect_identical(nrow(result$dataset_name), 4L)  # All data should remain

  # 7. Test `get_stats` function for data counts and missing values
  stats <- cb_filter.datetime_range.tblist(
    source = data_object, variable = "date_var", range = c(as.POSIXct("2023-01-01"), Inf),
    dataset = "dataset_name"
  )$get_stats(data_object)
  expect_identical(stats$n_data, 3L)       # Count of non-NA entries
  expect_identical(stats$n_missing, 1L)    # Count of NA entries

  # 8. Test `get_defaults` function for range limits
  filter <- cb_filter.datetime_range.tblist(
    source = data_object, variable = "date_var",
    dataset = "dataset_name"
  )
  cache_object <- list(frequencies = data.frame(l_bound = min(data$date_var, na.rm = TRUE),
                                                u_bound = max(data$date_var, na.rm = TRUE)))
  defaults <- filter$get_defaults(data_object, cache_object)
  expect_identical(defaults$range, c(min(data$date_var, na.rm = TRUE), max(data$date_var, na.rm = TRUE)))

  # 9. Test if filtered attribute is correctly set on data after filtering
  result <- apply_filter(range = c(as.POSIXct("2023-01-02"), as.POSIXct("2023-01-03")), keep_na = TRUE)
  expect_true(attr(result$dataset_name, "filtered"))

})

test_that("datetime_range high level test", {
  # Create test data with date and value columns
  data <- data.frame(
    date_var = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-02 12:00:00", "2023-01-03 00:00:00", NA)),
    value = 1L:4L
  )

  # Set up source and apply a datetime_range filter from 2023-01-01 12:00:00 to 2023-01-02 12:00:00
  source <- set_source(
    tblist(data = data)
  ) %>%
    add_step(
      filter(
        "datetime_range", id = "date_var", dataset = "data",
        variable = "date_var", range = c("2023-01-01 12:00:00", "2023-01-02 12:00:00"),
        active = TRUE
      )
    )

  # Initialize cohort and apply filter steps
  coh <- cohortBuilder::cohort(source)
  coh$run_flow()

  # Get filtered data
  filtered_data <- coh$get_data("1")$data

  # Define expected data (rows in range or NA)
  expected_data <- data %>%
    dplyr::filter(date_var >= "2023-01-01 12:00:00" & date_var <= "2023-01-02 12:00:00" | is.na(date_var))

  # Check if filtered data matches expected data, ignoring attributes
  expect_equal(filtered_data, expected_data, ignore_attr = TRUE)
})
