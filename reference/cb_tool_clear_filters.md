# Create a tool for resetting filters to defaults

Returns a
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
that resets filter values to their defaults without removing the filters
from the cohort.

## Usage

``` r
cb_tool_clear_filters(cohort)
```

## Arguments

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

A `cb_tool` object.
