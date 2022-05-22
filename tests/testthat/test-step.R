discrete_filter <- filter(
  type = "discrete", id = "age_filter", name = "Age", variable = "age", dataset = "patients", value = 50
)
discrete_filter_two <- filter(
  type = "discrete", id = "age_filter_two", name = "Age", variable = "age", dataset = "patients", value = 50
)
patients_source <- set_source(
  tblist(patients = data.frame(id = 1:2, age = 50:51))
)

test_that("Registering steps and filters works for various filter-step combinations", {
  no_source <- register_steps_and_filters()
  expect_equal(no_source, list())

  source_no_steps_and_filters <- register_steps_and_filters(patients_source)
  expect_equal(no_source, list())

  no_step_one_filter <- register_steps_and_filters(
    patients_source,
    discrete_filter
  )
  expect_equal(step_filter_state(no_step_one_filter), list("1" = 1))

  expect_error(
    register_steps_and_filters(
      patients_source,
      discrete_filter,
      discrete_filter
    ),
    "Cannot create filters with the same id in a single step."
  )

  no_step_two_filters <- register_steps_and_filters(
    patients_source,
    discrete_filter,
    discrete_filter_two
  )
  expect_equal(step_filter_state(no_step_two_filters), list("1" = 2))

  one_step_one_filter <- register_steps_and_filters(
    patients_source,
    step(
      discrete_filter
    )
  )
  expect_equal(step_filter_state(one_step_one_filter), list("1" = 1))
  expect_setequal(unname(no_step_one_filter), unname(one_step_one_filter))
  expect_equal(names(no_step_one_filter), names(one_step_one_filter))

  expect_error(
    register_steps_and_filters(
      patients_source,
      step(
        discrete_filter,
        discrete_filter
      )
    ),
    "Cannot create filters with the same id in a single step."
  )

  one_step_two_filters <- register_steps_and_filters(
    patients_source,
    step(
      discrete_filter,
      discrete_filter_two
    )
  )
  expect_equal(step_filter_state(one_step_two_filters), list("1" = 2))
  expect_setequal(unname(one_step_two_filters), unname(no_step_two_filters))
  expect_equal(names(one_step_two_filters), names(no_step_two_filters))

  two_steps <- register_steps_and_filters(
    patients_source,
    step(
      discrete_filter,
      discrete_filter_two
    ),
    step(discrete_filter)
  )
  expect_equal(step_filter_state(two_steps), list("1" = 2, "2" = 1))

  #?? here
  filter_in_source <- patients_source$clone() %>%
    add_filter(discrete_filter)
  one_filter_in_source <- register_steps_and_filters(filter_in_source)

  expect_equal(step_filter_state(one_filter_in_source), list("1" = 1))
  expect_setequal(unname(one_filter_in_source), unname(one_step_one_filter))
  expect_equal(names(one_filter_in_source), names(one_step_one_filter))

  same_filters_in_source <- patients_source$clone() %>%
    add_filter(discrete_filter) %>%
    add_filter(discrete_filter)
  expect_error(
    register_steps_and_filters(same_filters_in_source),
    "Cannot create filters with the same id in a single step."
  )

  filters_in_source <- patients_source$clone() %>%
    add_filter(discrete_filter) %>%
    add_filter(discrete_filter_two)
  two_filters_in_source <- register_steps_and_filters(filters_in_source)

  expect_equal(step_filter_state(two_filters_in_source), list("1" = 2))
  expect_setequal(unname(two_filters_in_source), unname(one_step_two_filters))
  expect_equal(names(two_filters_in_source), names(one_step_two_filters))

  filters_in_two_steps <- patients_source$clone() %>%
    add_filter(discrete_filter, "1") %>%
    add_filter(discrete_filter_two, "1") %>%
    add_filter(discrete_filter, "2")
  filters_in_two_sources_reg <- register_steps_and_filters(filters_in_two_steps)

  expect_equal(step_filter_state(filters_in_two_sources_reg), list("1" = 2, "2" = 1))
  expect_setequal(unname(filters_in_two_sources_reg), unname(two_steps))
  expect_equal(names(filters_in_two_sources_reg), names(two_steps))

  step_in_source <- patients_source$clone() %>%
    add_step(step(discrete_filter, discrete_filter_two))
  step_in_source_req <- register_steps_and_filters(step_in_source)

  expect_equal(step_filter_state(step_in_source_req), list("1" = 2))
  expect_setequal(unname(step_in_source_req), unname(one_step_two_filters))
  expect_equal(names(step_in_source_req), names(one_step_two_filters))

  steps_in_source <- patients_source$clone() %>%
    add_step(step(discrete_filter, discrete_filter_two)) %>%
    add_step(step(discrete_filter))
  steps_in_source_req <- register_steps_and_filters(steps_in_source)

  expect_equal(step_filter_state(steps_in_source_req), list("1" = 2, "2" = 1))
  expect_setequal(unname(steps_in_source_req), unname(two_steps))
  expect_equal(names(steps_in_source_req), names(two_steps))
})
