# Create a tool that adds filters and sets their values in one call

Returns a
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
that combines filter addition and value assignment into a single tool
call. This avoids issues with LLMs splitting the work across multiple
parallel calls.

## Usage

``` r
cb_tool_apply_filters(cohort)
```

## Arguments

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

A `cb_tool` object.
