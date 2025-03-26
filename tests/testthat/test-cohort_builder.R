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
  variable = "actor_id", dataset = "actor", range = c(1, 10)
)
range_filter_actor_two <- filter(
  type = "range", id = "actor_filter_two", name = "Actor",
  variable = "actor_id", dataset = "actor", range = c(3, 6)
)
range_filter_actor_three <- filter(
  type = "range", id = "actor_filter_three", name = "Actor",
  variable = "actor_id", dataset = "actor", range = c(6, 8)
)
discrete_filter_film <- filter(
  type = "discrete", id = "film_filter", name = "Film",
  variable = "rating", dataset = "film", value = c("G","R")
)
discrete_filter_film_two <- filter(
  type = "discrete", id = "film_filter", name = "Film",
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
  expect_identical(get_state(coh, 2L)[[1L]]$filters[[2L]]$id, "film_filter")

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

