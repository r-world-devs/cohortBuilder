# Get Cohort related statistics.

Display data statistics related to specified step or filter.

## Usage

``` r
stat(x, step_id, filter_id, ..., state = "post")
```

## Arguments

- x:

  Cohort object.

- step_id:

  When \`filter_id\` specified, \`step_id\` precises from which step the
  filter comes from. Otherwise data from specified step is used to
  calculate required statistics.

- filter_id:

  If not missing, filter related data statistics are returned.

- ...:

  Specific parameters passed to filter related method.

- state:

  Should the stats be calculated on data before ("pre") or after
  ("post") filtering in specified step.

## Value

List of filter-specific values summing up underlying filter data.

## See also

[cohort-methods](https://r-world-devs.github.io/cohortBuilder/reference/cohort-methods.md)
