# Create a tool returning row counts per dataset and step

Returns a
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
that reports how many rows remain in each dataset at each step (before
and after filtering). This is the primary tool for understanding the
impact of applied filters.

## Usage

``` r
cb_tool_get_data_summary(cohort)
```

## Arguments

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

A `cb_tool` object.
