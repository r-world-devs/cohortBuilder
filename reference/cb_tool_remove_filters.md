# Create a tool for removing filters from the cohort

Returns a
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
that removes filters from a step in the cohort. If removing all filters
from a step, the entire step is removed.

## Usage

``` r
cb_tool_remove_filters(cohort)
```

## Arguments

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

A `cb_tool` object.
