load(testthat::test_path("../data/sakila/sakila.rda"))
sakila_source <- set_source(as.tblist(sakila))
sakila_source$binding_keys <- bind_keys(
  bind_key(update = data_key('actor', 'actor_id'),
           data_key('film_actor', 'actor_id')
  ),
  bind_key(update = data_key('film_actor','film_id'),
           data_key('film','film_id')
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
  variable = "rating", dataset = "film", value = c("G","R")
)
discrete_filter_film_two <- filter(
  type = "discrete", id = "film_filter_two", name = "Film",
  variable = "rating", dataset = "film", value = c("G")
)

step_1 <- step(range_filter_actor,discrete_filter_film)
step_2 <- step(range_filter_actor_two,discrete_filter_film_two)
step_3 <- step(range_filter_actor_three)


test_that("Add new step works fine",{
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

test_that("Get state works fine",{
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
