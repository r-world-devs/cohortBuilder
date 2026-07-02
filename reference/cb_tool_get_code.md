# Create a tool returning reproducible filtering code

Returns a
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
that generates reproducible R code for the current cohort filtering
pipeline via `get_code()`.

## Usage

``` r
cb_tool_get_code(cohort)
```

## Arguments

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

A `cb_tool` object.
