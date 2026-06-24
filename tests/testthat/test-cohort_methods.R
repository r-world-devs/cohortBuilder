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
  variable = "Sepal.Length", dataset = "iris", range = c(5L, 6L)
)
range_iris_two <- filter(
  type = "range", id = "sepal_l_two", name = "Sepal.Length.Two",
  variable = "Sepal.Length", dataset = "iris", range = c(9L, 11L)
)
patients_source <- set_source(
  tblist(patients = data.frame(id = 1L:2L, age = 50L:51L))
)

load(testthat::test_path("../data/sakila/sakila.rda"))
sakila_source <- set_source(as.tblist(sakila))
sakila_source$binding_keys <- bind_keys(
  bind_key(
    update = data_key("actor", "actor_id"),
    data_key("film_actor", "actor_id")
  ),
  bind_key(
    update = data_key("film_actor", "film_id"),
    data_key("film", "film_id")
  )
)

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
  expect_identical(coh$get_data(1L, state = "pre")$iris, iris)

  coh$run_flow()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$iris$Species), c("setosa", "virginica"))
  expect_setequal(collapse::funique(coh$get_data(2L, state = "post")$iris$Species), c("virginica"))

  # Using S3 Cohort method
  coh <- Cohort$new(
    iris_source,
    step(discrete_iris_one),
    step(discrete_iris_two)
  )
  expect_identical(coh$get_data(1L, state = "pre")$iris, iris)

  coh <- coh |> run()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$iris$Species), c("setosa", "virginica"))
  expect_setequal(collapse::funique(coh$get_data(2L, state = "post")$iris$Species), c("virginica"))
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
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())

  # Using S3 Cohort methods
  coh <- Cohort$new()
  iris_source <- set_source(
    tblist(iris = iris)
  )
  coh <- coh |> add_source(iris_source)
  state <- coh$sum_up_state()
  expect_true(state$source)
  expect_null(state$source_vars)
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())
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
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

  coh$run_flow()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$iris$Species), c("setosa", "virginica"))

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
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 2L))
  expect_identical(state$steps_structure, list("1" = c("species_filter", "species_filter_two")))

  coh$run_flow()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$iris$Species), c("virginica"))

  # Using S3 Cohort methods
  coh <- Cohort$new()
  iris_source <- set_source(
    tblist(iris = iris)
  )
  coh <- coh |> add_source(iris_source)
  coh <- coh |> add_step(
    step(
      discrete_iris_one
    )
  )
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

  coh <- coh |> run()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$iris$Species), c("setosa", "virginica"))

  coh <- Cohort$new()
  iris_source <- set_source(
    tblist(iris = iris)
  )
  coh <- coh |>
    add_source(iris_source) |>
    add_step(
      step(
        discrete_iris_one,
        discrete_iris_two
      )
    )
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 2L))
  expect_identical(state$steps_structure, list("1" = c("species_filter", "species_filter_two")))

  coh <- coh |> run()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$iris$Species), c("virginica"))
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
  expect_identical(state$n_steps, 2L)
  expect_identical(state$n_filters, list("1" = 1L, "2" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter", "2" = "species_filter_two"))

  coh$run_flow()
  expect_setequal(collapse::funique(coh$get_data(2L, state = "post")$iris$Species), c("virginica"))

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
  expect_identical(state$n_steps, 2L)
  expect_identical(state$n_filters, list("1" = 1L, "2" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter", "2" = "species_filter_two"))
  expect_setequal(collapse::funique(coh$get_data(2L, state = "post")$iris$Species), c("virginica"))

  # Using S3 Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )
  coh <- coh |> add_step(
    step(
      discrete_iris_two
    )
  )
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 2L)
  expect_identical(state$n_filters, list("1" = 1L, "2" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter", "2" = "species_filter_two"))

  coh <- coh |> run()
  expect_setequal(collapse::funique(coh$get_data(2L, state = "post")$iris$Species), c("virginica"))

  ## auto-run flow
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )
  coh <- coh |> add_step(
    step(
      discrete_iris_two
    ),
    run_flow = TRUE
  )
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 2L)
  expect_identical(state$n_filters, list("1" = 1L, "2" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter", "2" = "species_filter_two"))
  expect_setequal(collapse::funique(coh$get_data(2L, state = "post")$iris$Species), c("virginica"))
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
  coh$remove_step(2L)
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

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
  coh$remove_step(1L)
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter_two"))

  ## 1 step and removing it
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    )
  )
  coh$remove_step(1L)
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())

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
  coh <- coh |> rm_step(2L)
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

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
  coh <- coh |> rm_step(1L)
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter_two"))

  ## 1 step and removing it
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    )
  )
  coh <- coh |> rm_step(1L)
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())
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
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

  # Using S3 Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    )
  )
  coh <- coh |>
    add_filter(
      discrete_iris_one
    )

  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))
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
  coh$remove_filter(1L, "species_filter_two")
  state <- coh$sum_up_state()
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

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
  coh$remove_filter(1L, "species_filter")
  state <- coh$sum_up_state()
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter_two"))

  # 1 filter and removing it
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    )
  )
  coh <- coh |> rm_filter(1L, "species_filter")
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())

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
  coh <- coh |> rm_filter(1L, "species_filter_two")
  state <- coh$sum_up_state()
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter"))

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
  coh <- coh |> rm_filter(1L, "species_filter")
  state <- coh$sum_up_state()
  expect_identical(state$n_filters, list("1" = 1L))
  expect_identical(state$steps_structure, list("1" = "species_filter_two"))

  # 1 filter and removing it
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    step(
      discrete_iris_one
    )
  )
  coh <- coh |> rm_filter(1L, "species_filter")
  state <- coh$sum_up_state()
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())
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
  coh$update_filter(1L, "species_filter_two", value = "setosa")
  coh$run_flow()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$iris$Species), "setosa")

  coh$update_filter(1L, "sepal_l", variable = "Petal.Length", range = c(1.0, 1.5))
  coh$run_flow()
  var_range <- range(coh$get_data(1L, state = "post")$iris$Petal.Length)
  expect_true(var_range[1L] >= 1L && var_range[2L] <= 1.5)

  expect_warning(
    coh$update_filter(1L, "sepal_l", type = "discrete"),
    label = "Cannot modify filter ‘type’, ‘id’, ‘name’ parameters."
  )

  expect_warning(
    coh$update_filter(1L, "sepal_l", active = "FALSE"),
    regexp = "Active accepts only logical values."
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
  coh <- coh |> update_filter(1L, "species_filter_two", value = "setosa")
  coh <- coh |> run()
  expect_setequal(collapse::funique(coh$get_data(1L, state = "post")$iris$Species), "setosa")

  coh <- coh |> update_filter(1L, "sepal_l", variable = "Petal.Length", range = c(1.0, 1.5))
  coh <- coh |> run()
  var_range <- range(coh$get_data(1L, state = "post")$iris$Petal.Length)
  expect_true(var_range[1L] >= 1L && var_range[2L] <= 1.5)

  expect_warning(
    coh$update_filter(1L, "sepal_l", type = "discrete"),
    label = "Cannot modify filter ‘type’, ‘id’, ‘name’ parameters."
  )

  expect_warning(
    coh$update_filter(1L, "sepal_l", active = "FALSE"),
    regexp = "Active accepts only logical values."
  )
})

test_that("Updating source works fine", {
  iris2 <- iris
  iris2[150L, 1L] <- 10L
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
  expect_identical(coh$get_data(1L, state = "pre")$iris, iris2)
  expect_true(state$source, TRUE)
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())

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
  expect_identical(coh$get_data(1L, state = "pre")$iris, iris2)
  expect_true(state$source, TRUE)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 2L))
  expect_identical(state$steps_structure, list("1" = c("species_filter", "sepal_l_two")))

  coh$run_flow()
  expect_identical(coh$get_data(1L, state = "post")$iris$Sepal.Length, 10.0)

  iris2 <- iris
  iris2[150L, 1L] <- 10L
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
  coh <- coh |> update_source(new_source, keep_steps = FALSE)

  state <- coh$sum_up_state()
  expect_identical(coh$get_data(1L, state = "pre")$iris, iris2)
  expect_true(state$source, TRUE)
  expect_identical(state$n_steps, 0L)
  expect_identical(state$n_filters, 0L)
  expect_identical(state$steps_structure, list())

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
  coh <- coh |> update_source(new_source, keep_steps =  TRUE)

  state <- coh$sum_up_state()
  expect_identical(coh$get_data(1L, state = "pre")$iris, iris2)
  expect_true(state$source, TRUE)
  expect_identical(state$n_steps, 1L)
  expect_identical(state$n_filters, list("1" = 2L))
  expect_identical(state$steps_structure, list("1" = c("species_filter", "sepal_l_two")))

  coh <- coh |> run()
  expect_identical(coh$get_data(1L, state = "post")$iris$Sepal.Length, 10.0)
})

test_that("Getting filter stats works fine", {
  # Using direct Cohort methods
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )
  expect_identical(coh$get_stats(1L, "species_filter", state = "pre")$choices, as.list(table(iris$Species)))
  coh$run_flow()
  expect_identical(
    coh$get_stats(1L, "species_filter", state = "post")$choices,
    as.list(table(iris$Species[iris$Species %in% c("setosa", "virginica")]))
  )

  expect_error(
    stat(coh, 10L),
    regexp = "Step is not exist in this cohort object."
  )

  # Using S3 Cohort methods
  coh <- cohort(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )

  expect_identical(stat(coh, 1L, "species_filter", state = "pre")$choices, as.list(table(iris$Species)))
  coh <- coh |> run()
  expect_identical(
    stat(coh, 1L, "species_filter", state = "post")$choices,
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
  expect_identical(coh$get_cache("1", "species_filter", state = "pre")$choices,
                   list(setosa = 50L, versicolor = 50L, virginica = 50L))
  expect_identical(coh$get_cache("1", "species_filter", state = "pre")$n_data, 150L)
  expect_identical(coh$get_cache("1", "species_filter", state = "pre")$n_missing, 0L)

  expect_identical(coh$get_cache("1", "species_filter", state = "post")$choices,
                   list(setosa = 50L, versicolor = 0L, virginica = 50L))
  expect_identical(coh$get_cache("1", "species_filter", state = "post")$n_data, 100L)
  expect_identical(coh$get_cache("1", "species_filter", state = "post")$n_missing, 0L)
})

test_that("get_cache returns fresh post-stats after re-run when caching is disabled", {
  # Regression: with cache = FALSE, run_step does not refresh stored stats.
  # A post-stat read lazily before a re-run used to leave a stale value in
  # private$cache, so the next get_cache() returned pre-run numbers.
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one,
    cache = FALSE
  )

  coh$run_flow()

  # Lazily populate the cache with the first run's post-stats.
  expect_identical(
    coh$get_cache("1", "species_filter", state = "post")$choices,
    list(setosa = 50L, versicolor = 0L, virginica = 50L)
  )

  # Narrow the filter and re-run; post-stats must reflect the new value.
  coh$update_filter(1L, "species_filter", value = "setosa")
  coh$run_flow()

  expect_identical(
    coh$get_cache("1", "species_filter", state = "post")$choices,
    list(setosa = 50L, versicolor = 0L, virginica = 0L)
  )
  expect_identical(coh$get_cache("1", "species_filter", state = "post")$n_data, 50L)

  # Source stats (cache_id "0") must remain stable across re-runs.
  expect_identical(
    coh$get_cache("1", "species_filter", state = "pre")$choices,
    list(setosa = 50L, versicolor = 50L, virginica = 50L)
  )
})

test_that("Bind keys work fine", {
  patients <- data.frame(
    id = letters[1L:3L], name = c("a", "b", "b"),
    surname = c("A", "A", "B"), surname2 = c("A", "A", "B"),
    age = 1L:3L, stringsAsFactors = FALSE
  )
  treatment <- data.frame(
    id = letters[1L:3L], name = c("a", "b", "b"),
    surname = c("A", "A", "B"), treatment = LETTERS[1L:3L],
    stringsAsFactors = FALSE
  )


  # directed relation graph (update != "all"), single key
  coh <- Cohort$new(
    set_source(
      tblist(patients = patients, treatment = treatment),
      binding_keys = bind_keys(
        bind_key(update = data_key("treatment", "id"), data_key("patients", "id"))
      )
    ),
    filter("range", id = "patients", name = "Patients", variable = "age", range = c(1L, 2L), dataset = "patients"),
    filter("discrete", id = "treatment", name = "Treatment", variable = "treatment", value = NA, dataset = "treatment")
  )

  coh$run_flow()

  expect_equal(
    coh$get_data("1", state = "post")$treatment,
    treatment[1L:2L, ],
    ignore_attr = TRUE
  )
  expect_true(
    attr(coh$get_data("1", state = "post")$treatment, "filtered")
  )

  expect_identical(coh$get_cache("1", "treatment", state = "post")$choices, list("A" = 1L, "B" = 1L))
  expect_identical(coh$get_cache("1", "treatment", state = "post")$n_data, 2L)
  expect_identical(coh$get_cache("1", "treatment", state = "post")$n_missing, 0L)

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
    filter("range", id = "patients", name = "Patients", variable = "age", range = c(1L, 2L), dataset = "patients"),
    filter("discrete", id = "treatment", name = "Treatment", variable = "treatment", value = NA, dataset = "treatment")
  )

  coh$run_flow()

  expect_equal(coh$get_data("1", state = "post")$treatment, treatment[1L:2L, ], ignore_attr = TRUE)
  expect_true(
    attr(coh$get_data("1", state = "post")$treatment, "filtered")
  )

  expect_identical(coh$get_cache("1", "treatment", state = "post")$choices, list("A" = 1L, "B" = 1L))
  expect_identical(coh$get_cache("1", "treatment", state = "post")$n_data, 2L)
  expect_identical(coh$get_cache("1", "treatment", state = "post")$n_missing, 0L)

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
    filter("range", id = "patients", name = "Patients", variable = "age", range = c(1L, 2L), dataset = "patients"),
    filter("discrete", id = "treatment", name = "Treatment", variable = "treatment", value = NA, dataset = "treatment")
  )

  coh$run_flow()

  expect_equal(coh$get_data("1", state = "post")$treatment, treatment[1L:2L, ], ignore_attr = TRUE)
  expect_true(
    attr(coh$get_data("1", state = "post")$treatment, "filtered")
  )

  expect_identical(coh$get_cache("1", "treatment", state = "post")$choices, list("A" = 1L, "B" = 1L))
  expect_identical(coh$get_cache("1", "treatment", state = "post")$n_data, 2L)
  expect_identical(coh$get_cache("1", "treatment", state = "post")$n_missing, 0L)

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
    filter("range", id = "patients", name = "Patients", variable = "age", range = c(1L, 2L), dataset = "patients"),
    filter("discrete", id = "treatment", name = "Treatment", variable = "treatment", value = NA, dataset = "treatment")
  )

  coh$run_flow()

  expect_equal(coh$get_data("1", state = "post")$treatment, treatment[1L:2L, ], ignore_attr = TRUE)
  expect_true(
    attr(coh$get_data("1", state = "post")$treatment, "filtered")
  )
  expect_true(
    attr(coh$get_data("1", state = "post")$patients, "filtered")
  )

  expect_identical(coh$get_cache("1", "treatment", state = "post")$choices, list("A" = 1L, "B" = 1L))
  expect_identical(coh$get_cache("1", "treatment", state = "post")$n_data, 2L)
  expect_identical(coh$get_cache("1", "treatment", state = "post")$n_missing, 0L)
})

test_that("Defining and accessing description works fine", {
  # Using direct Cohort methods
  species_filter_no_desc <- filter("discrete", id = "species", dataset = "iris", variable = "Species")
  species_filter_desc <- filter("discrete", id = "species", dataset = "iris",
                                variable = "Species", description = "Species Filter")
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris),
      description = list(iris = "Iris dataset.")
    ),
    step(species_filter_no_desc),
    step(species_filter_desc)
  )
  expect_identical(
    coh$show_help("iris"),
    "Iris dataset."
  )
  expect_identical(
    description(coh, "iris"),
    "Iris dataset."
  )

  expect_null(
    coh$show_help(filter_id = "species", step_id = "1")
  )
  expect_null(
    description(coh, filter_id = "species", step_id = "1")
  )

  expect_identical(
    coh$show_help(filter_id = "species", step_id = "2"),
    "Species Filter"
  )
  expect_identical(
    description(coh, filter_id = "species", step_id = "2"),
    "Species Filter"
  )
})

test_that("steps_range returns empty character when from is greater than to", {
  expect_identical(steps_range(3L, 2L), character(0L))
})

test_that("assign_filters_to_step returns empty list when no filters", {
  expect_identical(assign_filters_to_step(list(0L, id = "2")), list())
})

test_that("next_step returns the next index as a character string", {
  expect_identical(next_step("1"), "2")
  expect_type(next_step("1"), "character")
})

test_that("copy_step with step_id works correctly", {

  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one, range_iris_one),
    step(discrete_iris_two)
  )

  #set last id of step before using the function
  pre_last_step_id <- as.integer(coh$last_step_id())
  #get list of filters in first step
  list_of_filters <- get_state(coh, 1L)[[1L]]$filters

  coh$copy_step(1L)

  expect_false(is.null(coh$get_step(pre_last_step_id + 1L)))
  expect_identical(get_state(coh, coh$last_step_id())[[1L]]$filters, list_of_filters)
})

test_that("copy_step without step_id duplicates filters from last step", {

  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one, range_iris_one),
    step(discrete_iris_two)
  )

  #set last id of step before using the function
  pre_last_step_id <- as.integer(coh$last_step_id())
  #get list of filters in last step
  list_of_filters <- get_state(coh, pre_last_step_id)[[1L]]$filters

  coh$copy_step()

  expect_false(is.null(coh$get_step(pre_last_step_id + 1L)))
  expect_identical(get_state(coh, coh$last_step_id())[[1L]]$filters, list_of_filters)
})

test_that("copy_step duplicate selected filters without step_id", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one, range_iris_one),
    step(discrete_iris_two)
  )

  pre_last_step_id <- as.integer(coh$last_step_id())
  list_of_filters <- get_state(coh, 1L)[[1L]]$filters

  coh$copy_step(filters = coh$get_filter(1L))

  expect_false(is.null(coh$get_step(pre_last_step_id + 1L)))
  expect_identical(get_state(coh, coh$last_step_id())[[1L]]$filters, list_of_filters)
})

test_that("copy_step trigger data calculations works fine", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one, range_iris_one),
    step(discrete_iris_two)
  )

  list_of_filters <- get_state(coh, coh$last_step_id())[[1L]]$filters

  expect_null(get_data(coh))

  coh$copy_step(run_flow = TRUE)

  expect_false(is.null(get_data(coh)))
  expect_identical(get_state(coh, coh$last_step_id())[[1L]]$filters, list_of_filters)
})

test_that("remove_step with missing step_id remove last step", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one, range_iris_one),
    step(discrete_iris_two)
  )


  pre_last_step_id <- as.integer(coh$last_step_id())
  #set list of filters to ensure that only the last step is removed
  if (pre_last_step_id != 1L) {
    pre_list_of_filters <- get_state(coh, c(1L:pre_last_step_id - 1L))
  }

  coh$remove_step()

  expect_identical(coh$last_step_id(), as.character(pre_last_step_id - 1L))
  expect_null(coh$get_step(pre_last_step_id))

  if (pre_last_step_id != 1L) {
    expect_identical(get_state(coh, c(1L:pre_last_step_id - 1L)), pre_list_of_filters)
  }
})

test_that("remove_step trigger data calculations works fine", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one, range_iris_one),
    step(discrete_iris_two)
  )

  expect_null(get_data(coh))

  coh$remove_step(run_flow = TRUE)

  expect_false(is.null(get_data(coh)))
})

test_that("add_filter trigger data calculations works fine", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one)
  )

  expect_null(get_data(coh))

  coh$add_filter(range_iris_one, 1L, run_flow = TRUE)

  expect_false(is.null(get_data(coh)))
})

test_that("remove_filter trigger data calculations works fine", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one)
  )

  expect_null(get_data(coh))

  coh$remove_filter(1L, 1L, run_flow = TRUE)

  expect_false(is.null(get_data(coh)))
})

test_that("get_state returns state in JSON format correctly", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one)
  )

  expect_true(jsonlite::validate(get_state(coh, 1L, json = TRUE)))
})

test_that("Restoring cohort configurations works fine", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one)
  )
  # Character type
  pre_state_character <- get_state(coh)
  coh$add_filter(range_iris_one, 2L)
  expect_false(identical(get_state(coh), pre_state_character))
  restore(coh, pre_state_character)
  expect_identical(get_state(coh), pre_state_character)

  # JSON type
  pre_state_json <- get_state(coh, json = TRUE)
  coh$add_filter(range_iris_one, 2L)
  expect_false(identical(get_state(coh), pre_state_character))
  restore(coh, pre_state_json)
  expect_identical(get_state(coh), pre_state_character)
})

test_that("Restoring cohort configurations without state returns invisible FALSE", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one)
  )

  pre_state <- get_state(coh)

  coh$add_filter(range_iris_one, 2L)

  expect_false(identical(get_state(coh), pre_state))
  expect_invisible(coh$restore(state = NULL))
  expect_false(coh$restore(state = NULL))
  expect_false(identical(get_state(coh), pre_state))
})

test_that("Restoring cohort configurations trigger data calculations works fine", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one)
  )
  coh_2 <- coh$clone()
  run(coh_2)
  pre_state <- get_state(coh)

  coh$add_filter(range_iris_one, 2L)
  expect_false(identical(get_state(coh_2), get_state(coh)))
  expect_null(get_data(coh))

  restore(coh, pre_state, run_flow = TRUE)

  expect_false(is.null(get_data(coh)))
  expect_identical(get_state(coh), get_state(coh))
  expect_identical(get_data(coh), get_data(coh))
})

test_that("restore correctly restore filters filter type date_range and datetime_range", {
  coh <- Cohort$new(
    set_source(
      tblist(issues = librarian$issues)
    ),
    step(
      filter(
        "date_range", id = "issues_date", dataset = "issues",
        variable = "date", range = c(as.Date("2010-10-01"), as.Date("2015-10-01"))
      ),
      filter(
        "date_range", id = "issues_date2", dataset = "issues",
        variable = "date", range = as.Date(NULL)
      ),
      filter(
        "datetime_range", id = "issues_datetime", dataset = "issues",
        variable = "date", range = c(as.POSIXct("2010-10-01"), as.POSIXct("2015-10-01"))
      ),
      filter(
        "datetime_range", id = "issues_datetime2", dataset = "issues",
        variable = "date", range = as.POSIXct(NULL)
      )
    )
  )

  pre_state <- get_state(coh)

  coh$add_filter(range_iris_one, 2L)
  coh$remove_filter(1L, "issues_datetime")

  expect_false(identical(get_state(coh), pre_state))

  restore(coh, pre_state)

  expect_identical(get_state(coh), pre_state)
})

test_that("Verify that new custom class method works correctly", {
  .collect_data.custom_tblist <- function(source, data_object) {
    "custom_tblist_data_collect"
  }

  registerS3method(".collect_data", "custom_tblist", .collect_data.custom_tblist)

  coh <- cohort(
    set_source(
      tblist(iris = iris, .class = "custom_tblist")
    ),
    step(discrete_iris_one)
  )

  run(coh)
  expect_identical(get_data(coh, 1L, collect = TRUE), "custom_tblist_data_collect")
})

test_that("Verify that new verbose class method works correctly", {
  .collect_data.verbose <- function(source, data_object) {
    message(nrow(data_object[[1L]]))
    NextMethod()
  }

  registerS3method(".collect_data", "verbose", .collect_data.verbose)

  coh <- cohort(
    set_source(
      tblist(iris = iris, .class = "verbose")
    ),
    step(discrete_iris_one)
  )

  run(coh)
  expect_message(get_data(coh, 1L, collect = TRUE), regexp =  nrow(get_data(coh, 1L, collect = TRUE[[1L]])))
})

test_that("update_filter changed active status works fine", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one, discrete_iris_two)
  )
  first_filter_id <- "species_filter"
  second_filter_id <- "species_filter_two"

  #default TRUE
  expect_true(coh$get_filter(step_id = 1L, "species_filter")@active)
  expect_true(coh$get_filter(step_id = 1L, "species_filter_two")@active)

  coh$update_filter(1L, "species_filter", active = FALSE)

  expect_false(coh$get_filter(step_id = 1L, "species_filter")@active)
  expect_true(coh$get_filter(step_id = 1L, "species_filter_two")@active)

  coh$update_filter(1L, "species_filter", active = TRUE)

  expect_true(coh$get_filter(step_id = 1L, "species_filter")@active)
  expect_true(coh$get_filter(step_id = 1L, "species_filter_two")@active)
})

test_that("update_filter trigger data calculations works fine", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one)
  )

  expect_null(get_data(coh))

  #do not trigger data calculations without any changes
  coh$update_filter(1L, "species_filter", run_flow = TRUE)

  expect_null(get_data(coh))

  #trigger data calculations
  coh$update_filter(1L, "species_filter", active = FALSE, run_flow = TRUE)

  expect_false(is.null(get_data(coh)))
})

test_that("Create cohort object with triggered data calculations", {
  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one),
    run_flow = TRUE
  )

  expect_false(is.null(get_data(coh)))
})


test_that("Retrieving reproducible code works fine", {
  # Using direct Cohort methods

  # covr modifies function body so the test doesn't pass
  skip_on_covr()

  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    discrete_iris_one
  )
  repro_code <- coh$get_code(
    1L, "species_filter", mark_step = FALSE,
    include_methods = character(0L), include_ations = character(0L),
    output = FALSE, width.cutoff = 120L
  )
  target_code <- quote({
    source <- list(dtconn = tblist(iris = iris))
    data_object <- source$dtconn
    data_object[["iris"]] <- data_object[["iris"]] |>
      dplyr::filter(Species %in% c("setosa", "virginica", NA))
  })
  expect_identical(
    gsub("\n|\\s+", " ", repro_code$text.tidy),
    as.character(target_code)[-1L]
  )
})


test_that("code returns expression to create filtered tblist", {

  skip_on_covr()

  coh <- Cohort$new(
    set_source(
      tblist(iris = iris)
    ),
    step(discrete_iris_one, range_iris_one),
    step(discrete_iris_two),
    run_flow = TRUE
  )

  code_as_text <- code(coh, include_methods = NULL, include_action = NULL, mark_step = FALSE, output = FALSE)

  for (line in code_as_text$text.tidy) {
    rlang::eval_bare(rlang::parse_expr(line))
  }

  expect_equal(data_object$iris, get_data(coh)$iris, ignore_attr = "filtered")
  expect_output(code(coh))
})

# Tests for general cohort methods

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

  # Add step without data calculations: the new step is seeded with the parent's
  # post snapshot so it is renderable without a flow (and repeated adds work).
  add_step(coh, step_3)

  expect_length(get_state(coh), number_of_steps + 2L)
  # New step carries the parent's (step 2) post snapshot until it runs.
  expect_identical(nrow(get_data(coh)$actor), 4L)
  expect_true(coh$is_pending("3"))
  expect_identical(get_state(coh, 3L)[[1L]]$filters[[1L]]$id, "actor_filter_three")

  # Add empty step (without filters): also seeded from parent, no crash.
  add_step(coh, step())
  expect_length(get_state(coh), number_of_steps + 3L)
  expect_identical(nrow(get_data(coh)$actor), 4L)
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
  coh |> run()
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

  # Use an isolated temp-file device with display-list recording enabled so the
  # test does not depend on graphics state left by earlier tests and does not
  # write Rplots.pdf into the package. recordPlot() then reflects exactly what
  # plot_data() draws on this device.
  grDevices::pdf(tempfile(fileext = ".pdf"))
  grDevices::dev.control("enable")
  on.exit(while (!is.null(grDevices::dev.list())) grDevices::dev.off(), add = TRUE)

  # Nothing has been drawn yet, so the display list is empty.
  expect_length(grDevices::recordPlot()[[1L]], 0L)
  plot_data(coh, 1L, 2L)
  # plot_data() drew to the device, so the display list is now non-empty.
  expect_gt(length(grDevices::recordPlot()[[1L]]), 0L)
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

test_that("Pending state is properly updated", {
  coh <- Cohort$new(
    sakila_source,
    step_1,
    run_flow = TRUE
  )
  expect_false(coh$get_step("1")$pending)
  coh$update_filter(1L, "film_filter", value = "R", run_flow = FALSE)
  expect_true(coh$get_step("1")$pending)
  coh$run_flow()
  expect_false(coh$get_step("1")$pending)
})

test_that("Computing cache on request works as expected", {
  coh <- Cohort$new(
    sakila_source,
    step_1,
    run_flow = FALSE
  )
  expect_null(coh$get_cache("1", state = "pre", .recalc_when_missing = FALSE))
  expect_false(is.null(coh$get_cache("1", state = "pre", .recalc_when_missing = TRUE)))
})

# -- Cache control tests ------------------------------------------------------

test_that("cache = FALSE skips update_cache in run_step", {
  iris_source <- set_source(tblist(iris = iris))
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    value = "setosa", domain = c("setosa", "versicolor", "virginica")
  )
  coh <- Cohort$new(iris_source, f, cache = FALSE)
  coh$run_flow()

  # Filtering still works
  result <- coh$get_data(1L, state = "post")$iris
  expect_true(all(result$Species == "setosa"))

  # But no filter-level cache computed during run_step
  expect_null(coh$get_cache("1", "sp", state = "post", .recalc_when_missing = FALSE))
})

test_that("cache = TRUE (default) computes cache normally", {
  iris_source <- set_source(tblist(iris = iris))
  f <- filter(
    type = "discrete", id = "sp", variable = "Species", dataset = "iris",
    value = "setosa"
  )
  coh <- Cohort$new(iris_source, f)
  coh$run_flow()

  expect_false(is.null(coh$get_cache("1", "sp", state = "post", .recalc_when_missing = FALSE)))
})

# -- get_cache(name=) single-statistic access --------------------------------

test_that("get_cache(name=) returns the bare statistic matching $accessor", {
  coh <- Cohort$new(
    set_source(tblist(iris = iris)),
    step(filter(
      type = "discrete", id = "sp", variable = "Species", dataset = "iris",
      value = "setosa"
    ))
  )
  coh$run_flow()

  expect_identical(
    coh$get_cache("1", "sp", state = "post", name = "choices"),
    coh$get_cache("1", "sp", state = "post")$choices
  )
  expect_identical(
    coh$get_cache("1", "sp", state = "post", name = "n_data"),
    coh$get_cache("1", "sp", state = "post")$n_data
  )
})

test_that("get_cache(name=) computes only the requested statistic when missing", {
  coh <- Cohort$new(
    set_source(tblist(iris = iris)),
    step(filter(
      type = "discrete", id = "sp", variable = "Species", dataset = "iris",
      value = "setosa"
    ))
  )
  coh$run_flow()

  # Drop the stored entry, then request a single statistic.
  priv <- coh$.__enclos_env__$private
  priv$cache[["1"]]$filters$sp <- NULL

  choices <- coh$get_cache("1", "sp", state = "post", name = "choices")
  expect_false(is.null(choices))

  # Only "choices" (plus the bookkeeping "changed") was computed; the other
  # statistics were not.
  stored <- priv$cache[["1"]]$filters$sp
  expect_true("choices" %in% names(stored))
  expect_false("n_data" %in% names(stored))
  expect_false("n_missing" %in% names(stored))

  # Requesting another statistic adds it without recomputing the rest.
  expect_identical(coh$get_cache("1", "sp", state = "post", name = "n_data"), 50L)
  expect_true(all(c("choices", "n_data") %in% names(priv$cache[["1"]]$filters$sp)))
})

test_that("get_cache(name=) returns fresh values after re-run when cache disabled", {
  coh <- Cohort$new(
    set_source(tblist(iris = iris)),
    step(filter(
      type = "discrete", id = "sp", variable = "Species", dataset = "iris",
      value = c("setosa", "versicolor", "virginica")
    )),
    cache = FALSE
  )
  coh$run_flow()

  # Populate lazily, then narrow and re-run.
  invisible(coh$get_cache("1", "sp", state = "post", name = "choices"))
  coh$update_filter(1L, "sp", value = "setosa")
  coh$run_flow()

  # Both ordering of named reads must return fresh values.
  expect_identical(coh$get_cache("1", "sp", state = "post", name = "n_data"), 50L)
  expect_identical(
    coh$get_cache("1", "sp", state = "post", name = "choices"),
    list(setosa = 50L, versicolor = 0L, virginica = 0L)
  )
})

# -- is_cached invariant after add_step copies parent cache -------------------

# add_step() seeds the new step's slot from its parent (cache[new] <- cache[parent])
# so the step is renderable without a run. The cache slot is shared: cache[N] is
# both step N's post and step N+1's pre. run_step()'s is_cached guard must keep
# each stored entry consistent with a direct (cache-free) recomputation, and must
# never let the seeded copy wipe a parent's filter stats during a partial run.
# These tests assert the stored cache always matches a fresh recomputation.

# Recompute a filter's stats directly from the relevant data_object, bypassing
# the cache entirely.
direct_filter_stats <- function(coh, step_id, filter_id, state) {
  priv <- coh$.__enclos_env__$private
  data_id <- as.character(step_id)
  if (state == "pre") data_id <- cohortBuilder:::prev_step(step_id)
  cb_get_filter_stats(
    priv$steps[[as.character(step_id)]]$filters[[filter_id]],
    priv$source,
    priv$data_objects[[data_id]]
  )
}

# For every stored (non-NULL) filter-level cache entry, assert it equals a direct
# recomputation. Returns the number of entries actually checked.
expect_cache_consistent <- function(coh) {
  priv <- coh$.__enclos_env__$private
  checked <- 0L
  for (sid in names(priv$steps)) {
    for (fid in names(priv$steps[[sid]]$filters)) {
      for (state in c("pre", "post")) {
        stored <- coh$get_cache(sid, fid, state = state, .recalc_when_missing = FALSE)
        if (is.null(stored)) next
        direct <- direct_filter_stats(coh, sid, fid, state)
        stored <- stored[setdiff(names(stored), "changed")]
        expect_identical(
          stored[names(direct)], direct[names(direct)],
          info = sprintf("step=%s filter=%s state=%s", sid, fid, state)
        )
        checked <- checked + 1L
      }
    }
  }
  checked
}

mk_species <- function(value) {
  filter(
    type = "discrete", id = "sp", name = "Species", variable = "Species",
    dataset = "iris", value = value
  )
}

test_that("is_cached keeps cache consistent across progressive add_step (single run_flow)", {
  coh <- Cohort$new(
    set_source(tblist(iris = iris)),
    step(mk_species(c("setosa", "versicolor", "virginica")))
  )
  coh$add_step(step(mk_species(c("setosa", "versicolor"))))
  coh$add_step(step(mk_species("setosa")))
  coh$run_flow()

  expect_gt(expect_cache_consistent(coh), 0L)
})

test_that("is_cached keeps cache consistent when each add_step runs its own flow", {
  coh <- Cohort$new(
    set_source(tblist(iris = iris)),
    step(mk_species(c("setosa", "versicolor", "virginica")))
  )
  coh$run_flow()
  coh$add_step(step(mk_species(c("setosa", "versicolor"))))
  coh$run_flow()
  coh$add_step(step(mk_species("setosa")))
  coh$run_flow()

  expect_gt(expect_cache_consistent(coh), 0L)
})

test_that("add_step + partial run_flow(min_step) does not corrupt parent cache", {
  # The seeded copy cache[2] <- cache[1] must not leak into step 1's stored
  # stats, and the step-level pre update during run_step must not wipe step 1's
  # filter caches.
  coh <- Cohort$new(
    set_source(tblist(iris = iris)),
    step(mk_species(c("setosa", "versicolor", "virginica")))
  )
  coh$run_flow()
  coh$add_step(step(mk_species("setosa")))
  coh$run_flow(min_step = 2L)

  expect_cache_consistent(coh)
  # Parent (step 1) post must remain the full three-species table.
  expect_identical(
    coh$get_cache("1", "sp", state = "post", .recalc_when_missing = FALSE)$choices,
    list(setosa = 50L, versicolor = 50L, virginica = 50L)
  )
  # Step 2 post is narrowed to setosa only.
  expect_identical(
    coh$get_cache("2", "sp", state = "post", .recalc_when_missing = FALSE)$choices,
    list(setosa = 50L, versicolor = 0L, virginica = 0L)
  )
})

test_that("partial run with a multi-filter parent keeps every parent filter cache", {
  fa <- filter(
    type = "discrete", id = "a", name = "Species", variable = "Species",
    dataset = "iris", value = c("setosa", "versicolor", "virginica")
  )
  fb <- filter(
    type = "range", id = "b", name = "Sepal.Length", variable = "Sepal.Length",
    dataset = "iris", range = c(4, 8)
  )
  coh <- Cohort$new(set_source(tblist(iris = iris)), step(fa, fb))
  coh$run_flow()
  coh$add_step(step(mk_species("setosa")))
  coh$run_flow(min_step = 2L)

  priv <- coh$.__enclos_env__$private
  # cache[1] is step 1's post and step 2's pre, so running step 2 adds step 2's
  # filter ("sp") pre-stats here. The invariant is that the destructive
  # step-level pre update must not wipe step 1's own filters "a" and "b".
  expect_true(all(c("a", "b") %in% names(priv$cache[["1"]]$filters)))
  expect_cache_consistent(coh)
})

test_that("copy_step (duplicate filter id) keeps cache consistent", {
  coh <- Cohort$new(
    set_source(tblist(iris = iris)),
    step(mk_species(c("setosa", "virginica")))
  )
  coh$run_flow()
  coh$copy_step(1L)
  coh$run_flow()

  expect_gt(expect_cache_consistent(coh), 0L)
})

# -- .propagate_domains tests -------------------------------------------------

test_that(".propagate_domains.default is a no-op", {
  iris_source <- set_source(tblist(iris = iris))
  result <- .propagate_domains(iris_source, iris_source$dtconn, "1", NULL, mode = "data")
  expect_null(result)
})

test_that("propagate_domains = 'none' (default) does not propagate", {
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", id = "sp1", variable = "Species", dataset = "iris",
           value = c("setosa", "versicolor"))
  )
  step2 <- step(
    filter("discrete", id = "sp2", variable = "Species", dataset = "iris",
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, step2)
  coh$run_flow()

  # Domain unchanged — propagation disabled
  step2_filter <- coh$get_filter("2", "sp2")
  expect_identical(step2_filter@domain, c("setosa", "versicolor", "virginica"))
})

# -- mode = "data" tests ------------------------------------------------------

test_that("propagate_domains = 'data' narrows discrete domain", {
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", id = "sp1", variable = "Species", dataset = "iris",
           value = c("setosa", "versicolor"))
  )
  step2 <- step(
    filter("discrete", id = "sp2", variable = "Species", dataset = "iris",
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, step2, propagate_domains = "data")
  coh$run_flow()

  step2_filter <- coh$get_filter("2", "sp2")
  expect_true(all(step2_filter@domain %in% c("setosa", "versicolor")))
  expect_false("virginica" %in% step2_filter@domain)
})

test_that("propagate_domains = 'data' keeps a factor column's post-data intact", {
  # Regression: Species is a factor. Propagation used to store the narrowed
  # domain as a factor, which made the downstream discrete filter match nothing
  # (`c(factor, NA)` -> integer codes), collapsing post-stats to 0 / 0%.
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", id = "sp1", variable = "Species", dataset = "iris",
           value = c("setosa", "versicolor"))
  )
  step2 <- step(
    filter("discrete", id = "sp2", variable = "Species", dataset = "iris",
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, step2, propagate_domains = "data")
  coh$run_flow()

  # The propagated domain must be character, not a factor.
  expect_type(coh$get_filter("2", "sp2")@domain, "character")

  # Step 2 imposes no additional restriction, so its post-data must equal its
  # pre-data (100 setosa + versicolor rows), not collapse to zero.
  pre2 <- nrow(coh$get_data("2", state = "pre")$iris)
  post2 <- nrow(coh$get_data("2", state = "post")$iris)
  expect_equal(pre2, 100L)
  expect_equal(post2, pre2)
})

test_that("propagate_domains = 'data' narrows range domain", {
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("range", id = "sl1", variable = "Sepal.Length", dataset = "iris",
           range = c(5, 6))
  )
  step2 <- step(
    filter("range", id = "sl2", variable = "Sepal.Length", dataset = "iris",
           domain = c(4, 8))
  )
  coh <- Cohort$new(iris_source, step1, step2, propagate_domains = "data")
  coh$run_flow()

  step2_filter <- coh$get_filter("2", "sl2")
  expect_true(step2_filter@domain[1L] >= 5)
  expect_true(step2_filter@domain[2L] <= 6)
})

test_that("propagate_domains = 'data' skips filters without domain", {
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", id = "sp1", variable = "Species", dataset = "iris",
           value = "setosa")
  )
  step2 <- step(
    filter("discrete", id = "sp2", variable = "Species", dataset = "iris")
  )
  coh <- Cohort$new(iris_source, step1, step2, propagate_domains = "data")
  coh$run_flow()

  # No domain was set, so propagation should not add one
  expect_null(coh$get_filter("2", "sp2")@domain)
})

test_that("propagate_domains = 'data' does not affect last step", {
  iris_source <- set_source(tblist(iris = iris))
  f <- filter("discrete", id = "sp", variable = "Species", dataset = "iris",
              value = "setosa", domain = c("setosa", "versicolor", "virginica"))
  coh <- Cohort$new(iris_source, f, propagate_domains = "data")
  coh$run_flow()

  step1_filter <- coh$get_filter("1", "sp")
  expect_identical(step1_filter@domain, c("setosa", "versicolor", "virginica"))
})

test_that("propagate_domains = 'data' narrows a step added after a resolved parent", {
  # Regression: adding a step to a parent that already filters must narrow the
  # new step's domain immediately, not leave it at the parent's full domain.
  # run_step narrows the step it runs from its parent's snapshot, so running the
  # newly added step (run_flow(min_step = new_id)) narrows it from the resolved
  # parent without any special add_step handling.
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", id = "sp", variable = "Species", dataset = "iris",
           value = c("setosa", "versicolor"),
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, propagate_domains = "data")
  coh$run_flow()

  # Clone step 1 (the GUI add-step path). The new step must open narrowed.
  coh$copy_step(run_flow = TRUE)

  step2_filter <- coh$get_filter("2", "sp")
  expect_setequal(step2_filter@domain, c("setosa", "versicolor"))
  expect_false("virginica" %in% step2_filter@domain)

  # The new step imposes no extra restriction, so its post-data equals its
  # pre-data (the 100 rows the parent passed through), not zero.
  pre2 <- nrow(coh$get_data("2", state = "pre")$iris)
  post2 <- nrow(coh$get_data("2", state = "post")$iris)
  expect_equal(pre2, 100L)
  expect_equal(post2, pre2)
})

test_that("propagate_domains = 'data' defers narrowing when added step is not run", {
  # With a pending parent (e.g. run-button mode, parent not yet run), adding a
  # step without running it must NOT narrow: domains are recomputed only when the
  # step runs. Running the flow then narrows it from the (now computed) parent.
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", id = "sp", variable = "Species", dataset = "iris",
           value = c("setosa", "versicolor"),
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, propagate_domains = "data")
  # Do not run: step 1 stays pending.
  coh$copy_step(run_flow = FALSE)

  # Deferred: step 2 still carries the full domain while the parent is pending.
  expect_setequal(
    coh$get_filter("2", "sp")@domain,
    c("setosa", "versicolor", "virginica")
  )

  # Running the flow then narrows it as usual.
  coh$run_flow()
  expect_setequal(coh$get_filter("2", "sp")@domain, c("setosa", "versicolor"))
})

# -- mode = "cache" tests -----------------------------------------------------

test_that("propagate_domains = 'cache' narrows discrete domain from stats", {
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", id = "sp1", variable = "Species", dataset = "iris",
           value = c("setosa", "versicolor"))
  )
  step2 <- step(
    filter("discrete", id = "sp1", variable = "Species", dataset = "iris",
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, step2, propagate_domains = "cache")
  coh$run_flow()

  step2_filter <- coh$get_filter("2", "sp1")
  expect_true(all(step2_filter@domain %in% c("setosa", "versicolor")))
  expect_false("virginica" %in% step2_filter@domain)
})

test_that("propagate_domains = 'cache' does nothing when cache disabled", {
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", id = "sp1", variable = "Species", dataset = "iris",
           value = c("setosa", "versicolor"))
  )
  step2 <- step(
    filter("discrete", id = "sp2", variable = "Species", dataset = "iris",
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, step2,
                    cache = FALSE, propagate_domains = "cache")
  coh$run_flow()

  # No cache computed, so domain stays unchanged
  step2_filter <- coh$get_filter("2", "sp2")
  expect_identical(step2_filter@domain, c("setosa", "versicolor", "virginica"))
})

# -- mode = "filter" tests ----------------------------------------------------

test_that("propagate_domains = 'filter' narrows domain from filter value (no data access)", {
  iris_source <- set_source(tblist(iris = iris))
  # Filter ids are deterministic (dataset + variable), so the same filter keeps
  # the same id across steps and is matched during propagation.
  step1 <- step(
    filter("discrete", variable = "Species", dataset = "iris",
           value = c("setosa", "versicolor"))
  )
  step2 <- step(
    filter("discrete", variable = "Species", dataset = "iris",
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, step2, propagate_domains = "filter")
  coh$run_flow()

  step2_filter <- coh$get_filter("2", "iris-Species")
  expect_identical(step2_filter@domain, c("setosa", "versicolor"))
})

test_that("propagate_domains = 'filter' works with range filters", {
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("range", variable = "Sepal.Length", dataset = "iris",
           range = c(5, 6))
  )
  step2 <- step(
    filter("range", variable = "Sepal.Length", dataset = "iris",
           domain = c(4, 8))
  )
  coh <- Cohort$new(iris_source, step1, step2, propagate_domains = "filter")
  coh$run_flow()

  step2_filter <- coh$get_filter("2", "iris-SepalLength")
  expect_identical(step2_filter@domain, c(5, 6))
})

test_that("propagate_domains = 'filter' works with cache disabled", {
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", variable = "Species", dataset = "iris",
           value = c("setosa"))
  )
  step2 <- step(
    filter("discrete", variable = "Species", dataset = "iris",
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, step2,
                    cache = FALSE, propagate_domains = "filter")
  coh$run_flow()

  # filter mode doesn't need cache
  step2_filter <- coh$get_filter("2", "iris-Species")
  expect_identical(step2_filter@domain, "setosa")
})

test_that("propagate_domains = 'filter' skips when no matching filter in current step", {
  iris_source <- set_source(tblist(iris = iris))
  # Different variables produce different deterministic ids, so step 2's
  # Sepal.Length filter has no counterpart in step 1.
  step1 <- step(
    filter("discrete", variable = "Species", dataset = "iris",
           value = c("setosa"))
  )
  step2 <- step(
    filter("range", variable = "Sepal.Length", dataset = "iris",
           domain = c(4, 8))
  )
  coh <- Cohort$new(iris_source, step1, step2, propagate_domains = "filter")
  coh$run_flow()

  # No matching filter for Sepal.Length in step 1 — domain unchanged
  step2_filter <- coh$get_filter("2", "iris-SepalLength")
  expect_identical(step2_filter@domain, c(4, 8))
})

test_that("propagate_domains = 'filter' uses domain as value when filter is unset", {
  iris_source <- set_source(tblist(iris = iris))
  step1 <- step(
    filter("discrete", variable = "Species", dataset = "iris",
           domain = c("setosa", "versicolor"))
  )
  step2 <- step(
    filter("discrete", variable = "Species", dataset = "iris",
           domain = c("setosa", "versicolor", "virginica"))
  )
  coh <- Cohort$new(iris_source, step1, step2, propagate_domains = "filter")
  coh$run_flow()

  # Step 1 filter has value = NA, so cb_intersect_domain returns domain c("setosa", "versicolor")
  step2_filter <- coh$get_filter("2", "iris-Species")
  expect_identical(step2_filter@domain, c("setosa", "versicolor"))
})
