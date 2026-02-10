# Plot filter related Cohort data.

For specified filter the method calls filter-related plot method to
present data.

## Usage

``` r
plot_data(x, step_id, filter_id, ..., state = "post")
```

## Arguments

- x:

  Cohort object.

- step_id:

  Id of step in which the filter was defined..

- filter_id:

  Filter id.

- ...:

  Another parameters passed to filter plotting method.

- state:

  Generate plot based on data before ("pre") or after ("post")
  filtering.

## Value

Filter-specific plot.

## See also

[cohort-methods](https://r-world-devs.github.io/cohortBuilder/reference/cohort-methods.md)
