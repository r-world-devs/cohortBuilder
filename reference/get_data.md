# Get step related data

Get step related data

## Usage

``` r
get_data(x, step_id, state = "post", collect = FALSE)
```

## Arguments

- x:

  Cohort object.

- step_id:

  Id of the step from which to source data.

- state:

  Return data before ("pre") or after ("post") step filtering?

- collect:

  Return raw data source (\`FALSE\`) object or collected (to R memory)
  data (\`TRUE\`).

## Value

Subset of Source-specific data connection object or its evaluated
version.

## See also

[cohort-methods](https://r-world-devs.github.io/cohortBuilder/reference/cohort-methods.md)
