discrete_iris_one <- filter(
  type = "discrete", id = "species_filter", name = "Species",
  variable = "Species", dataset = "iris", value = c("setosa", "virginica")
)
discrete_iris_two <- filter(
  type = "discrete", id = "species_filter_two", name = "Species",
  variable = "Species", dataset = "iris", value = c("virginica")
)
range_iris_one <- filter(
  type = "range", id = "sepal_l", name = "Sepal.Length",
  variable = "Sepal.Length", dataset = "iris", range = c(5, 6)
)
range_iris_two <- filter(
  type = "range", id = "sepal_l_two", name = "Sepal.Length.Two",
  variable = "Sepal.Length", dataset = "iris", range = c(9, 11)
)
patients_source <- set_source(
  tblist(patients = data.frame(id = 1:2, age = 50:51))
)

test_that("Running steps filter raw data properly", {
  iris_source <- set_source(
    tblist(iris = iris)
  )

  # Using direct Cohort method
  coh <- Cohort$new(
    iris_source,
    step(discrete_iris_one),
    step(discrete_iris_two)
  )
  expect_equal(coh$get_data(1, state = "pre")$iris, iris)

  coh$run_flow()
  expect_setequal(unique(coh$get_data(1, state = "post")$iris$Species), c("setosa", "virginica"))
  expect_setequal(unique(coh$get_data(2, state = "post")$iris$Species), c("virginica"))

  # Using S3 Cohort method
  coh <- Cohort$new(
    iris_source,
    step(discrete_iris_one),
    step(discrete_iris_two)
  )
  expect_equal(coh$get_data(1, state = "pre")$iris, iris)

  coh <- coh %>% run()
  expect_setequal(unique(coh$get_data(1, state = "post")$iris$Species), c("setosa", "virginica"))
  expect_setequal(unique(coh$get_data(2, state = "post")$iris$Species), c("virginica"))
})

test_that("Adding source on empty cohort works fine", {
  # Using direct Cohort methods
  coh <- Cohort$new()
  iris_source <- set_source(
    tblist(iris = iris)
  )
  coh$add_source(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())

  # Using S3 Cohort methods
  coh <- Cohort$new()
  iris_source <- set_source(
    tblist(iris = iris)
  )
  coh <- coh %>% add_source(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_equal(state$source_vars, NULL)
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())
})

test_that("Adding step on source-only cohort works fine", {
  # Using direct Cohort methods
  coh <- Cohort$new()
  iris_source <- set_source(
    tblist(iris = iris)
  )
  coh$add_source(iris_source)
  coh$add_step(
    step(
      discrete_iris_one
    )
  )
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  coh$run_flow()
  expect_setequal(unique(coh$get_data(1, state = "post")$iris$Species), c("setosa", "virginica"))

  coh <- Cohort$new()
  iris_source <- set_source(
    tblist(iris = iris)
  )
  coh$add_source(iris_source)
  coh$add_step(
    step(
      discrete_iris_one,
      discrete_iris_two
    )
  )
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 2))
  expect_equal(state$steps_structure, list("1" = c("species_filter", "species_filter_two")))

  coh$run_flow()
  expect_setequal(unique(coh$get_data(1, state = "post")$iris$Species), c("virginica"))

  # Using S3 Cohort methods
  coh <- Cohort$new()
  iris_source <- set_source(
    tblist(iris = iris)
  )
  coh <- coh %>% add_source(iris_source)
  coh <- coh %>% add_step(
    step(
      discrete_iris_one
    )
  )
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  coh <- coh %>% run()
  expect_setequal(unique(coh$get_data(1, state = "post")$iris$Species), c("setosa", "virginica"))

  coh <- Cohort$new()
  iris_source <- set_source(
    tblist(iris = iris)
  )
  coh <- coh %>%
    add_source(iris_source) %>%
    add_step(
    step(
      discrete_iris_one,
      discrete_iris_two
    )
  )
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 2))
  expect_equal(state$steps_structure, list("1" = c("species_filter", "species_filter_two")))

  coh <- coh %>% run()
  expect_setequal(unique(coh$get_data(1, state = "post")$iris$Species), c("virginica"))
})

test_that("Adding step on existing cohort with step works fine", {
  # Using direct Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )
  coh$add_step(
    step(
      discrete_iris_two
    )
  )
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 2)
  expect_equal(state$n_filters, list("1" = 1, "2" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter", "2" = "species_filter_two"))

  coh$run_flow()
  expect_setequal(unique(coh$get_data(2, state = "post")$iris$Species), c("virginica"))

  ## auto-run flow
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )
  coh$add_step(
    step(
      discrete_iris_two
    ),
    run_flow = TRUE
  )
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 2)
  expect_equal(state$n_filters, list("1" = 1, "2" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter", "2" = "species_filter_two"))
  expect_setequal(unique(coh$get_data(2, state = "post")$iris$Species), c("virginica"))

  # Using S3 Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )
  coh <- coh %>% add_step(
    step(
      discrete_iris_two
    )
  )
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 2)
  expect_equal(state$n_filters, list("1" = 1, "2" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter", "2" = "species_filter_two"))

  coh <- coh %>% run()
  expect_setequal(unique(coh$get_data(2, state = "post")$iris$Species), c("virginica"))

  ## auto-run flow
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )
  coh <- coh %>% add_step(
    step(
      discrete_iris_two
    ),
    run_flow = TRUE
  )
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 2)
  expect_equal(state$n_filters, list("1" = 1, "2" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter", "2" = "species_filter_two"))
  expect_setequal(unique(coh$get_data(2, state = "post")$iris$Species), c("virginica"))
})

test_that("Removing step works fine", {
  # Using direct Cohort methods
  ## >1 steps and remove last step
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    ),
    step(
      discrete_iris_two
    )
  )
  coh$remove_step(2)
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  ## >1 steps and remove first step
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    ),
    step(
      discrete_iris_two
    )
  )
  coh$remove_step(1)
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter_two"))

  ## 1 step and removing it
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    )
  )
  coh$remove_step(1)
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())

  # Using S3 methods on Cohort object
  ## >1 steps and remove last step
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    ),
    step(
      discrete_iris_two
    )
  )
  coh <- coh %>% rm_step(2)
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  ## >1 steps and remove first step
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    ),
    step(
      discrete_iris_two
    )
  )
  coh <- coh %>% rm_step(1)
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter_two"))

  ## 1 step and removing it
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    )
  )
  coh <- coh %>% rm_step(1)
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())
})

test_that("Adding filter works fine", {
  # Using direct Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    )
  )
  coh$add_filter(
    discrete_iris_one
  )
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  # Using S3 Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    )
  )
  coh <- coh %>%
    add_filter(
      discrete_iris_one
    )

  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))
})

test_that("Removing filter works fine", {
  # Using direct Cohort methods
  ## >1 filters in the step and removing the last one
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one,
      discrete_iris_two
    )
  )
  coh$remove_filter(1, "species_filter_two")
  state <- coh$sum_up_state()
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  # >1 filters in the step and removing the first one
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one,
      discrete_iris_two
    )
  )
  coh$remove_filter(1, "species_filter")
  state <- coh$sum_up_state()
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter_two"))

  # 1 filter and removing it
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    )
  )
  coh <- coh %>% rm_filter(1, "species_filter")
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())

  # Using S3 Cohort methods
  ## >1 filters in the step and removing the last one
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one,
      discrete_iris_two
    )
  )
  coh <- coh %>% rm_filter(1, "species_filter_two")
  state <- coh$sum_up_state()
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter"))

  # >1 filters in the step and removing the first one
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one,
      discrete_iris_two
    )
  )
  coh <- coh %>% rm_filter(1, "species_filter")
  state <- coh$sum_up_state()
  expect_equal(state$n_filters, list("1" = 1))
  expect_equal(state$steps_structure, list("1" = "species_filter_two"))

  # 1 filter and removing it
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    )
  )
  coh <- coh %>% rm_filter(1, "species_filter")
  state <- coh$sum_up_state()
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())
})

test_that("Updating filter works fine", {
  # Using direct Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      range_iris_one,
      discrete_iris_two
    )
  )
  coh$update_filter(1, "species_filter_two", value = "setosa")
  coh$run_flow()
  expect_setequal(unique(coh$get_data(1, state = "post")$iris$Species), "setosa")

  coh$update_filter(1, "sepal_l", variable = "Petal.Length", range = c(1, 1.5))
  coh$run_flow()
  var_range <- range(coh$get_data(1, state = "post")$iris$Petal.Length)
  expect_true(var_range[1] >= 1 && var_range[2] <= 1.5)

  expect_warning(
    coh$update_filter(1, "sepal_l", type = "discrete"),
    label = "Cannot modify filter ‘type’, ‘id’, ‘name’ parameters."
  )

  # Using S3 Cohort methods
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    step(
      range_iris_one,
      discrete_iris_two
    )
  )
  coh <- coh %>% update_filter(1, 'species_filter_two', value = "setosa")
  coh <- coh %>% run()
  expect_setequal(unique(coh$get_data(1, state = "post")$iris$Species), "setosa")

  coh <- coh %>% update_filter(1, "sepal_l", variable = "Petal.Length", range = c(1, 1.5))
  coh <- coh %>% run()
  var_range <- range(coh$get_data(1, state = "post")$iris$Petal.Length)
  expect_true(var_range[1] >= 1 && var_range[2] <= 1.5)

  expect_warning(
    coh$update_filter(1, "sepal_l", type = "discrete"),
    label = "Cannot modify filter ‘type’, ‘id’, ‘name’ parameters."
  )
})

test_that("Updating source works fine", {
  iris2 <- iris
  iris2[150, 1] <- 10
  new_source <- set_source(
    tblist(iris = iris2)
  )

  # Using direct Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one,
      range_iris_one
    )
  )

  ## setting up new source with erasing steps
  coh$update_source(new_source, keep_steps = FALSE)

  state <- coh$sum_up_state()
  expect_equal(coh$get_data(1, state = "pre")$iris, iris2)
  expect_true(state$source, TRUE)
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())

  ## setting up new source with keeping steps unchanged
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one,
      range_iris_two
    )
  )
  coh$update_source(new_source, keep_steps =  TRUE)

  state <- coh$sum_up_state()
  expect_identical(coh$get_data(1, state = "pre")$iris, iris2)
  expect_true(state$source, TRUE)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 2))
  expect_equal(state$steps_structure, list("1" = c("species_filter", "sepal_l_two")))

  coh$run_flow()
  expect_equal(coh$get_data(1, state = "post")$iris$Sepal.Length, 10)

  iris2 <- iris
  iris2[150, 1] <- 10
  new_source <- set_source(
    tblist(iris = iris2)
  )

  # Using S3 Cohort methods
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one,
      range_iris_one
    )
  )

  ## setting up new source with erasing steps
  coh <- coh %>% update_source(new_source, keep_steps = FALSE)

  state <- coh$sum_up_state()
  expect_identical(coh$get_data(1, state = "pre")$iris, iris2)
  expect_true(state$source, TRUE)
  expect_equal(state$n_steps, 0)
  expect_equal(state$n_filters, 0)
  expect_equal(state$steps_structure, list())

  ## setting up new source with keeping steps unchanged
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one,
      range_iris_two
    )
  )
  coh <- coh %>% update_source(new_source, keep_steps =  TRUE)

  state <- coh$sum_up_state()
  expect_identical(coh$get_data(1, state = "pre")$iris, iris2)
  expect_true(state$source, TRUE)
  expect_equal(state$n_steps, 1)
  expect_equal(state$n_filters, list("1" = 2))
  expect_equal(state$steps_structure, list("1" = c("species_filter", "sepal_l_two")))

  coh <- coh %>% run()
  expect_equal(coh$get_data(1, state = "post")$iris$Sepal.Length, 10)
})

test_that("Getting filter stats works fine", {
  # Using direct Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )
  expect_equal(coh$get_stats(1, "species_filter", state = "pre")$choices, as.list(table(iris$Species)))
  coh$run_flow()
  expect_equal(
    coh$get_stats(1, "species_filter", state = "post")$choices,
    as.list(table(iris$Species[iris$Species %in% c("setosa", "virginica")]))
  )

  # Using S3 Cohort methods
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )

  expect_equal(stat(coh, 1, "species_filter", state = "pre")$choices, as.list(table(iris$Species)))
  coh <- coh %>% run()
  expect_equal(
    stat(coh, 1, "species_filter", state = "post")$choices,
    as.list(table(iris$Species[iris$Species %in% c("setosa", "virginica")]))
  )
})

test_that("Caching works fine", {
  # Using direct Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )

  coh$run_flow()
  expect_equal(coh$get_cache("1", "species_filter", state = "pre")$choices, list(setosa = 50, versicolor = 50, virginica = 50))
  expect_equal(coh$get_cache("1", "species_filter", state = "pre")$n_data, 150)
  expect_equal(coh$get_cache("1", "species_filter", state = "pre")$n_missing, 0)

  expect_equal(coh$get_cache("1", "species_filter", state = "post")$choices, list(setosa = 50, versicolor = 0, virginica = 50))
  expect_equal(coh$get_cache("1", "species_filter", state = "post")$n_data, 100)
  expect_equal(coh$get_cache("1", "species_filter", state = "post")$n_missing, 0)
})

test_that("Bind keys work fine", {
  patients <- data.frame(
    id = letters[1:3], name = c("a", "b", "b"),
    surname = c("A", "A", "B"), surname2 = c("A", "A", "B"), age = 1:3
  )
  treatment <- data.frame(
    id = letters[1:3], name = c("a", "b", "b"),
    surname = c("A", "A", "B"), treatment = LETTERS[1:3]
  )


  # directed relation graph (update != "all"), single key
  coh <- Cohort$new(
    set_source(
      tblist(patients = patients, treatment = treatment),
      binding_keys = bind_keys(
        bind_key(update = data_key("treatment", "id"), data_key("patients", "id"))
      )
    ),
    filter("range", id = "patients", name = "Patients", variable = "age", range = c(1, 2), dataset = "patients"),
    filter("discrete", id = "treatment", name = "Treatment", variable = "treatment", value = NA, dataset = "treatment")
  )

  coh$run_flow()

  expect_equal(
    coh$get_data("1", state = "post")$treatment,
    treatment[1:2, ],
    ignore_attr = TRUE
  )
  expect_true(
    attr(coh$get_data("1", state = "post")$treatment, "filtered")
  )

  expect_equal(coh$get_cache("1", "treatment", state = "post")$choices, list("A" = 1, "B" = 1))
  expect_equal(coh$get_cache("1", "treatment", state = "post")$n_data, 2)
  expect_equal(coh$get_cache("1", "treatment", state = "post")$n_missing, 0)

  # directed relation graph (update != "all"), multi key with same names
  coh <- Cohort$new(
    set_source(
      tblist(patients = patients, treatment = treatment),
      binding_keys = bind_keys(
        bind_key(
          update = data_key("treatment", c("name", "surname")),
          data_key("patients", c("name", "surname"))
        )
      )
    ),
    filter("range", id = "patients", name = "Patients", variable = "age", range = c(1, 2), dataset = "patients"),
    filter("discrete", id = "treatment", name = "Treatment", variable = "treatment", value = NA, dataset = "treatment")
  )

  coh$run_flow()

  expect_equal(coh$get_data("1", state = "post")$treatment, treatment[1:2, ], ignore_attr = TRUE)
  expect_true(
    attr(coh$get_data("1", state = "post")$treatment, "filtered")
  )

  expect_equal(coh$get_cache("1", "treatment", state = "post")$choices, list("A" = 1, "B" = 1))
  expect_equal(coh$get_cache("1", "treatment", state = "post")$n_data, 2)
  expect_equal(coh$get_cache("1", "treatment", state = "post")$n_missing, 0)

  # directed relation graph (update != "all"), multi key with different names
  coh <- Cohort$new(
    set_source(
      tblist(patients = patients, treatment = treatment),
      binding_keys = bind_keys(
        bind_key(
          update = data_key("treatment", c("name", "surname")),
          data_key("patients", c("name", "surname2"))
        )
      )
    ),
    filter("range", id = "patients", name = "Patients", variable = "age", range = c(1, 2), dataset = "patients"),
    filter("discrete", id = "treatment", name = "Treatment", variable = "treatment", value = NA, dataset = "treatment")
  )

  coh$run_flow()

  expect_equal(coh$get_data("1", state = "post")$treatment, treatment[1:2, ], ignore_attr = TRUE)
  expect_true(
    attr(coh$get_data("1", state = "post")$treatment, "filtered")
  )

  expect_equal(coh$get_cache("1", "treatment", state = "post")$choices, list("A" = 1, "B" = 1))
  expect_equal(coh$get_cache("1", "treatment", state = "post")$n_data, 2)
  expect_equal(coh$get_cache("1", "treatment", state = "post")$n_missing, 0)

  # cyclic relation graph, single key
  coh <- Cohort$new(
    set_source(
      tblist(patients = patients, treatment = treatment),
      binding_keys = bind_keys(
        bind_key(
          update = data_key("patients", "id"),
          data_key("treatment", "id")
        ),
        bind_key(
          update = data_key("treatment", "id"),
          data_key("patients", "id")
        )
      )
    ),
    filter("range", id = "patients", name = "Patients", variable = "age", range = c(1, 2), dataset = "patients"),
    filter("discrete", id = "treatment", name = "Treatment", variable = "treatment", value = NA, dataset = "treatment")
  )

  coh$run_flow()

  expect_equal(coh$get_data("1", state = "post")$treatment, treatment[1:2, ], ignore_attr = TRUE)
  expect_true(
    attr(coh$get_data("1", state = "post")$treatment, "filtered")
  )
  expect_true(
    attr(coh$get_data("1", state = "post")$patients, "filtered")
  )

  expect_equal(coh$get_cache("1", "treatment", state = "post")$choices, list("A" = 1, "B" = 1))
  expect_equal(coh$get_cache("1", "treatment", state = "post")$n_data, 2)
  expect_equal(coh$get_cache("1", "treatment", state = "post")$n_missing, 0)
})

test_that("Defining and accessing description works fine", {
  # Using direct Cohort methods
  species_filter_no_desc <- filter("discrete", id = "species", dataset = "iris", variable = "Species")
  species_filter_desc <- filter("discrete", id = "species", dataset = "iris", variable = "Species", description = "Species Filter")
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris),
      description = list(iris = "Iris dataset.")
    ),
    step(species_filter_no_desc),
    step(species_filter_desc)
  )
  expect_equal(
    coh$show_help("iris"),
    "Iris dataset."
  )
  expect_equal(
    description(coh, "iris"),
    "Iris dataset."
  )

  expect_equal(
    coh$show_help(filter_id = "species", step_id = "1"),
    NULL
  )
  expect_equal(
    description(coh, filter_id = "species", step_id = "1"),
    NULL
  )

  expect_equal(
    coh$show_help(filter_id = "species", step_id = "2"),
    "Species Filter"
  )
  expect_equal(
    description(coh, filter_id = "species", step_id = "2"),
    "Species Filter"
  )
})

# if (!covr::in_covr()) { # covr modifies function body so the test doesn't pass
#   test_that("(experimental) Retrieving reproducible code works fine", {
#     # Using direct Cohort methods
#     coh <- Cohort$new(
#       set_source(
#         tblist(iris = iris)
#       ),
#       discrete_iris_one
#     )
#     repro_code <- coh$get_code(1, "species_filter")
#     target_code <- quote({
#       data_object <- source$datasets
#       if (!identical(c("setosa", "virginica"), NA)) {
#         data_object[["iris"]] <- data_object[["iris"]] %>% dplyr::filter(!!sym("Species") %in% !!c("setosa", "virginica"))
#       }
#     })
#     expect_equal(
#       as.character(repro_code),
#       as.character(target_code)
#     )
#   })
# }
