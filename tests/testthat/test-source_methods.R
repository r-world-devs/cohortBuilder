discrete_filter_species <- filter(
  type = "discrete", id = "species_filter", name = "Species",
  variable = "Species", dataset = "iris", value = c("setosa", "virginica")
)
discrete_filter_species_two <- filter(
  type = "discrete", id = "species_filter_two", name = "Species",
  variable = "Species", dataset = "iris", value = c("setosa", "virginica")
)
patients_source <- set_source(
  tblist(patients = data.frame(id = 1:2, age = 50:51)),
  extra_param_one = "extra parameter",
  extra_param_two = "extra parameter"
)

test_that("Calling tblist type source returns valid structure list", {
  expect_equal(class(patients_source), c("tblist", "Source", "R6"))
  expect_equal(names(patients_source$attributes), c("extra_param_one", "extra_param_two"))
})

test_that("Adding step on source works fine", {
  # one step
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_step(
    step(
      discrete_filter_species
    )
  )
  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  # two steps
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_step(
    step(
      discrete_filter_species
    )
  ) %>% add_step(
    step(
      discrete_filter_species
    )
  )
  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 2)
  expect_equal(state$n_filters, list("1" = 1, "2" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter", "2" = "species_filter"))
})

test_that("Removing step on source works fine", {
  # removing the last one step
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_step(
    step(
      discrete_filter_species
    )
  )

  iris_source <- iris_source %>%
    rm_step(1)
  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())

  # two steps
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_step(
    step(
      discrete_filter_species
    )
  ) %>% add_step(
    step(
      discrete_filter_species
    )
  )

  no_last_step <- iris_source$clone() %>%
    rm_step()
  coh <- Cohort$new(no_last_step)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  no_last_step_specified <- iris_source$clone() %>%
    rm_step(2)
  coh <- Cohort$new(no_last_step_specified)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  # checking if steps are renamed
  no_first_step <- iris_source$clone() %>%
    rm_step(1)
  coh <- Cohort$new(no_first_step)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))
})

test_that("Adding filter on source works fine and attaches it to correct step", {
  # no step_id provided, should be 1 then
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species
  )
  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  # step_id provided
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species,
    step_id = 1
  )
  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  # multiple filters in the same (latest) step
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species
  ) %>% add_filter(
    discrete_filter_species_two
  )

  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 2))
  expect_equal(state$steps_structure, list("1" = c("species_filter", "species_filter_two")))

  # multiple filters in different steps
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species,
    step_id = 1
  ) %>% add_filter(
    discrete_filter_species,
    step_id = 2
  )

  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 2)
  expect_equal(state$n_filters, list("1" = 1, "2" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter", "2" = "species_filter"))
})

test_that("Removing filter on source works fine", {
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species
  )

  removed_last_filter <- iris_source$clone() %>%
    rm_filter(1, "species_filter")
  coh <- Cohort$new(removed_last_filter)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())

  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species
  ) %>% add_filter(
    discrete_filter_species_two
  )
  removed_filter_but_not_last_one <- iris_source$clone() %>%
    rm_filter(1, "species_filter_two")
  coh <- Cohort$new(removed_filter_but_not_last_one)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  removed_first_filter_in_step <- iris_source$clone() %>%
    rm_filter(1, "species_filter")
  coh <- Cohort$new(removed_first_filter_in_step)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter_two"))
})

test_that("Updating filter on source works fine", {
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species
  )
  iris_source <- iris_source %>%
    update_filter(1, "species_filter", value = "setosa")
  coh <- Cohort$new(iris_source)
  coh$run_flow()
  expect_setequal(coh$get_data(1, state = "post")$iris$Species, "setosa")
})

