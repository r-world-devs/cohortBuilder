# Create filtering step

Steps all to perform multiple stages of Source data filtering.

## Usage

``` r
step(...)
```

## Arguments

- ...:

  Filters. See
  [filter](https://r-world-devs.github.io/cohortBuilder/reference/filter.md).

## Value

List of class \`cb_step\` storing filters configuration.

## Examples

``` r
iris_step_1 <- step(
  filter('discrete', dataset = 'iris', variable = 'Species', value = 'setosa'),
  filter('discrete', dataset = 'iris', variable = 'Petal.Length', range = c(1.5, 2))
)
iris_step_2 <- step(
  filter('discrete', dataset = 'iris', variable = 'Sepal.Length', range = c(5, 10))
)

# Add step directly to Cohort
iris_source <- set_source(tblist(iris = iris))
coh <- iris_source |>
  cohort(
    iris_step_1,
    iris_step_2
  ) |>
  run()

nrow(get_data(coh, step_id = 1)$iris)
#> [1] 50
nrow(get_data(coh, step_id = 2)$iris)
#> [1] 50

# Add step to Cohort using add_step method
coh <- iris_source |>
  cohort()
coh <- coh |>
  add_step(iris_step_1) |>
  add_step(iris_step_2) |>
  run()
```
