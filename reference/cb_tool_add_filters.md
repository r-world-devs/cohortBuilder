# Create a tool for adding filters to a cohort

Returns a
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
that adds selected filters from the source's `available_filters` to the
cohort. The LLM chooses whether to add to a new step or the existing
last step via the `action` argument.

## Usage

``` r
cb_tool_add_filters(cohort)
```

## Arguments

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

A `cb_tool` object.
