# Create a tool returning the current cohort state

Returns a
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
whose function takes no arguments and returns a structured text summary
of all steps, filters, their active status, and pending state.

## Usage

``` r
cb_tool_describe_state(cohort)
```

## Arguments

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

A `cb_tool` object.
