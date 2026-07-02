# Create a tool returning available filters metadata

Returns a
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
whose function takes no arguments and returns a JSON string with filter
metadata from
[`shape`](https://r-world-devs.github.io/cohortBuilder/reference/shape.md).

## Usage

``` r
cb_tool_filters_meta(cohort)
```

## Arguments

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

A `cb_tool` object.
