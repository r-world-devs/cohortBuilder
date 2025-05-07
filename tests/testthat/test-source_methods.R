discrete_filter_species <- filter(
  type = "discrete", id = "species_filter", name = "Species",
  variable = "Species", dataset = "iris", value = c("setosa", "virginica")
)
discrete_filter_species_two <- filter(
  type = "discrete", id = "species_filter_two", name = "Species",
  variable = "Species", dataset = "iris", value = c("setosa", "virginica")
)
patients_source <- set_source(
  tblist(patients = data.frame(id = 1L:2L, age = 50L:51L)),
  extra_param_one = "extra parameter",
  extra_param_two = "extra parameter"
)

test_that("Calling tblist type source returns valid structure list", {
  expect_s3_class(patients_source, c("tblist", "Source", "R6"))
  expect_named(patients_source$attributes, c("extra_param_one", "extra_param_two"))
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
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

  # two steps
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>%
    add_step(
      step(
        discrete_filter_species
      )
    ) %>%
    add_step(
      step(
        discrete_filter_species
      )
    )
  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 2L)
  expect_identical(state$n_filters, list("1" = 1L, "2" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter", "2" = "species_filter"))
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
    rm_step(1L)
  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())

  # two steps
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>%
    add_step(
      step(
        discrete_filter_species
      )
    ) %>%
    add_step(
      step(
        discrete_filter_species
      )
    )

  no_last_step <- iris_source$clone() %>%
    rm_step()
  coh <- Cohort$new(no_last_step)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

  no_last_step_specified <- iris_source$clone() %>%
    rm_step(2L)
  coh <- Cohort$new(no_last_step_specified)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

  # checking if steps are renamed
  no_first_step <- iris_source$clone() %>%
    rm_step(1L)
  coh <- Cohort$new(no_first_step)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))
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
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

  # step_id provided
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species,
    step_id = 1L
  )
  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

  # multiple filters in the same (latest) step
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>%
    add_filter(
      discrete_filter_species
    ) %>%
    add_filter(
      discrete_filter_species_two
    )

  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 2L))
  expect_identical(state$steps_structure, list("1" = c("species_filter", "species_filter_two")))

  # multiple filters in different steps
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>%
    add_filter(
      discrete_filter_species,
      step_id = 1L
    ) %>%
    add_filter(
      discrete_filter_species,
      step_id = 2L
    )

  coh <- Cohort$new(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 2L)
  expect_identical(state$n_filters, list("1" = 1L, "2" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter", "2" = "species_filter"))
})

test_that("Removing filter on source works fine", {
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species
  )

  removed_last_filter <- iris_source$clone() %>%
    rm_filter(1L, "species_filter")
  coh <- Cohort$new(removed_last_filter)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())

  iris_source <- set_source(
    tblist(iris = iris)
  ) %>%
    add_filter(
      discrete_filter_species
    ) %>%
    add_filter(
      discrete_filter_species_two
    )
  removed_filter_not_last_one <- iris_source$clone() %>%
    rm_filter(1L, "species_filter_two")
  coh <- Cohort$new(removed_filter_not_last_one)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

  removed_first_filter_in_step <- iris_source$clone() %>%
    rm_filter(1L, "species_filter")
  coh <- Cohort$new(removed_first_filter_in_step)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter_two"))
})

test_that("Updating filter on source works fine", {
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species
  )
  iris_source <- iris_source %>%
    update_filter(1L, "species_filter", value = "setosa")
  coh <- Cohort$new(iris_source)
  coh$run_flow()
  expect_setequal(coh$get_data(1L, state = "post")$iris$Species, "setosa")
})

test_that("Removing step with ID '0' triggers warning", {
  iris_source <- set_source(
    tblist(iris = iris)
  ) %>% add_filter(
    discrete_filter_species
  )

  expect_warning(iris_source$rm_step("0"), "No steps to remove or wrong ID passed")
})

test_that("Initialize source with primary key set attribute primary_key", {
  key <- 1L
  iris_source <- set_source(
                            tblist(iris = iris),
                            primary_keys = key)

  type_of_primary_key <- typeof(key)
  expect_identical(iris_source$primary_keys, key)
  expect_type(iris_source$primary_keys, type_of_primary_key)
  expect_false(is.null(iris_source$primary_keys))
})

test_that("get returns attributes of source", {
  iris_source <- set_source(
                            tblist(iris = iris),
                            atribute1 = "test")

  expect_type(iris_source$get("atribute1"), "character")
  expect_type(iris_source$get("atribute2"), "NULL")
})
