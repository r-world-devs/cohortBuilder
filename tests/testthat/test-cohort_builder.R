load(testthat::test_path("../data/sakila/sakila.rda"))
sakila_source <- set_source(as.tblist(sakila))
sakila_source$binding_keys <- bind_keys(
                                        bind_key(update = data_key("actor", "actor_id"),
                                          data_key("film_actor", "actor_id")
                                        ),
                                        bind_key(update = data_key("film_actor", "film_id"),
                                          data_key("film", "film_id")
                                        ))

range_filter_actor <- filter(
  type = "range", id = "actor_filter", name = "Actor",
  variable = "actor_id", dataset = "actor", range = c(1L, 10L)
)
range_filter_actor_two <- filter(
  type = "range", id = "actor_filter_two", name = "Actor",
  variable = "actor_id", dataset = "actor", range = c(3L, 6L)
)
range_filter_actor_three <- filter(
  type = "range", id = "actor_filter_three", name = "Actor",
  variable = "actor_id", dataset = "actor", range = c(6L, 8L)
)
discrete_filter_film <- filter(
  type = "discrete", id = "film_filter", name = "Film",
  variable = "rating", dataset = "film", value = c("G", "R")
)
discrete_filter_film_two <- filter(
  type = "discrete", id = "film_filter_two", name = "Film",
  variable = "rating", dataset = "film", value = c("G")
)
datetamie_rage_filter <- filter(
  type = "datetime_range", id = "rental_filter_datetime_range", name = "Rental",
  variable = "return_date", dataset = "rental", range = as.POSIXct(c("2005-05-25 11:30:37", "2005-05-27 20:35:37"))
)
date_rage_filter <- filter(
  type = "date_range", id = "payment_filter_datetime_range", name = "Payment",
  variable = "payment_date", dataset = "payment", range = as.Date(c("2005-05-25", "2005-08-25"))
)

step_1 <- step(range_filter_actor, discrete_filter_film)
step_2 <- step(range_filter_actor_two, discrete_filter_film_two)
step_3 <- step(range_filter_actor_three)


test_that("Add new step works fine", {
  coh <- Cohort$new(
    sakila_source,
    step_1,
    run_flow = TRUE
  )
  number_of_steps <- 1L

  expect_identical(nrow(get_data(coh)$actor), 10L)
  expect_length(get_state(coh), number_of_steps)

  add_step(coh, step_2, run_flow = TRUE)

  expect_length(get_state(coh), number_of_steps + 1L)
  expect_identical(nrow(get_data(coh)$actor), 4L)
  expect_identical(get_state(coh, 2L)[[1L]]$filters[[1L]]$id, "actor_filter_two")
  expect_identical(get_state(coh, 2L)[[1L]]$filters[[2L]]$id, "film_filter_two")

  # Add step without data calculations
  add_step(coh, step_3)

  expect_length(get_state(coh), number_of_steps + 2L)
  expect_null(get_data(coh))
  expect_identical(get_state(coh, 3L)[[1L]]$filters[[1L]]$id, "actor_filter_three")

  # Add empty step (without filters)
  add_step(coh, step())
  expect_length(get_state(coh), number_of_steps + 3L)
  expect_null(get_data(coh))
  expect_error(add_step(coh))
})

test_that("Get state works fine", {
  test_type <- "range"
  test_id <- "actor_filter"
  test_name <- "Actor"
  test_variable <- "actor_id"
  test_dataset <- "actor"
  test_range <- c(1L, 10L)
  test_description <- "Description for filter"

  test_filter <- filter(
    type = test_type, id = test_id, name = test_name,
    variable = test_variable, dataset = test_dataset, range = test_range,
    description = test_description
  )

  coh <- Cohort$new(
    sakila_source,
    step(test_filter),
    step_2,
    run_flow = TRUE
  )
  number_of_steps <- 2L
  state_cohort <- get_state(coh)
  state_filters_step_1 <- state_cohort[[1L]]$filters[[1L]]

  expect_length(state_cohort, number_of_steps)
  expect_identical(state_filters_step_1$type, test_type)
  expect_identical(state_filters_step_1$id, test_id)
  expect_identical(state_filters_step_1$name, test_name)
  expect_identical(state_filters_step_1$variable, test_variable)
  expect_identical(state_filters_step_1$dataset, test_dataset)
  expect_identical(state_filters_step_1$range, test_range)
  expect_identical(state_filters_step_1$description, test_description)

  json_state <- get_state(coh, 1L, json = TRUE)
  expect_silent(jsonlite::fromJSON(json_state))

  state_from_json <- jsonlite::fromJSON(json_state)
  expect_identical(state_from_json$step, "1")

  expect_identical(state_from_json$filters[[1L]]$type, test_type)
  expect_identical(state_from_json$filters[[1L]]$id, test_id)
  expect_identical(state_from_json$filters[[1L]]$name, test_name)
  expect_identical(state_from_json$filters[[1L]]$variable, test_variable)
  expect_identical(state_from_json$filters[[1L]]$dataset, test_dataset)
  expect_identical(state_from_json$filters[[1L]]$range[[1L]], test_range)
  expect_identical(state_from_json$filters[[1L]]$description, test_description)
})

test_that("Update filter works fine", {
  coh <- Cohort$new(
    sakila_source,
    step_1,
    run_flow = TRUE
  )
  # Save all unique rating without filtering
  unique_rating <- collapse::funique(get_data(coh, 0L)$film$rating)

  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$film$rating), c("R", "G"))
  expect_setequal(get_state(coh, 1L)[[1L]]$filters[[2L]]$value, c("R", "G"))
  expect_true(get_state(coh, 1L)[[1L]]$filters[[2L]]$active)

  # Update film filter to one value with run_flow
  coh$update_filter(1L, "film_filter", value = "R", run_flow = TRUE)

  expect_identical(get_state(coh, 1L)[[1L]]$filters[[2L]]$value, "R")
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$film$rating), "R")

  # Update film filter to non-existent value without run_flow
  coh$update_filter(1L, "film_filter", value = "non_existent_vaule")

  expect_identical(get_state(coh, 1L)[[1L]]$filters[[2L]]$value, "non_existent_vaule")
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$film$rating), "R")
  coh %>% run()
  expect_identical(coh$get_data(1L, state = "post")$film$rating, character(0L))

  # Change active status
  coh$update_filter(1L, "film_filter", value = "G", active = FALSE, run_flow = TRUE)

  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$film$rating), unique_rating)
  expect_false(get_state(coh, 1L)[[1L]]$filters[[2L]]$active)

  expect_warning(
    coh$update_filter(1L, "film_filter", type = "range"),
    label = "Cannot modify filter ‘type’, ‘id’, ‘name’ parameters."
  )
  expect_warning(
    coh$update_filter(1L, "film_filter", id = "film_filter_two"),
    label = "Cannot modify filter ‘type’, ‘id’, ‘name’ parameters."
  )
  expect_warning(
    coh$update_filter(1L, "film_filter", name = "test"),
    label = "Cannot modify filter ‘type’, ‘id’, ‘name’ parameters."
  )
})

test_that("Restore state works fine", {
  step_datatime_range <- step(datetamie_rage_filter)
  step_date_range <- step(date_rage_filter)

  # Datetime
  sakila_source$dtconn$rental$return_date <- as.POSIXct(sakila_source$dtconn$rental$return_date)
  # Date
  sakila_source$dtconn$payment$payment_date <- as.Date(sakila_source$dtconn$payment$payment_date)

  coh <- Cohort$new(
    sakila_source,
    step_1,
    step_2,
    step_datatime_range,
    step_date_range,
    run_flow = TRUE
  )
  state_cohort <- get_state(coh)
  state_cohort_json <- get_state(coh, json = TRUE)
  expect_silent(jsonlite::fromJSON(state_cohort_json))
  data_cohort <- get_data(coh)

  coh$remove_step(2L)
  coh$remove_filter(1L, 1L, run_flow = TRUE)

  expect_false(identical(data_cohort, get_data(coh)))
  expect_false(identical(state_cohort, get_state(coh)))

  # Restore state with json = FALSE
  restore(coh, state_cohort, run_flow = TRUE)

  expect_identical(state_cohort, get_state(coh))
  expect_identical(data_cohort, get_data(coh))

  coh$remove_step()
  coh$remove_step()
  add_step(coh, step_3)
  state_cohort_2 <- get_state(coh)

  # Restore state with changed modifier
  restore(coh, state_cohort, modifier = function(prev_state, state) append(prev_state, state))

  identical(get_state(coh), append(state_cohort, state_cohort_2))
  expect_error(restore(coh, list("non_state")), regexp = ".*\\$ operator is invalid for atomic vectors.*")

  expect_false(identical(data_cohort, get_data(coh)))
  expect_false(identical(state_cohort_json, get_state(coh, json = TRUE)))

  # Restore state with json = TRUE
  restore(coh, state_cohort_json, run_flow = TRUE)

  expect_identical(state_cohort, get_state(coh))
  expect_identical(data_cohort, get_data(coh))
})

test_that("Plot data works fine", {
  coh <- Cohort$new(
    sakila_source,
    step_1,
    run_flow = TRUE
  )

  expect_error(recordPlot())
  plot_data(coh, 1L, 2L)
  expect_silent(recordPlot())
  dev.off()
})

test_that("Stats works fine", {
  coh <- Cohort$new(
    sakila_source,
    step_1,
    run_flow = TRUE
  )

  result_post <- coh$get_stats(1L, 2L)
  result_pre <- coh$get_stats(1L, 2L, state = "pre")

  expect_identical(result_post$n_data, nrow(coh$get_data(1L)$film))
  expect_identical(result_post$choices$G, sum(coh$get_data(1L)$film$rating == "G"))

  expect_identical(result_pre$n_data, nrow(coh$get_data(0L)$film))
  expect_identical(result_pre$choices$G, sum(coh$get_data(0L)$film$rating == "G"))
})

test_that("Remove_step works fine", {
  coh <- Cohort$new(
    sakila_source,
    step_1,
    step_2,
    run_flow = TRUE
  )
  id_first_filter <- coh$get_state()[[1L]]$filters[[1L]]$id
  number_of_steps_pre <- length(coh$get_state())
  # Remove step without step id remove last step
  coh$remove_step()

  expect_gt(number_of_steps_pre, length(coh$get_state()))
  expect_null(coh$get_step(2L))
  expect_identical(id_first_filter, coh$get_state()[[1L]]$filters[[1L]]$id)

  coh$add_step(step_2)

  # Remove first step
  coh$remove_step(1L)
  expect_false(identical(id_first_filter, coh$get_state()[[1L]]$filters[[1L]]$id))
  expect_null(coh$get_step(2L))
  expect_gt(number_of_steps_pre, length(coh$get_state()))

  coh$add_step(step_1)
  # Remove step with non-existed step id
  coh$remove_step(100L)
  expect_identical(number_of_steps_pre, length(coh$get_state()))
})
