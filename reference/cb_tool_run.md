# Create a tool for running the cohort pipeline

Returns a
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
that triggers data calculations for the entire cohort or a specific
step. Only functional when the `cb_tool_run_cohort` option is `FALSE`;
otherwise returns an informative message that the cohort runs
automatically.

## Usage

``` r
cb_tool_run(cohort)
```

## Arguments

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

A `cb_tool` object.
