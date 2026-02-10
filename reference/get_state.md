# Get Cohort configuration state.

Get Cohort configuration state.

## Usage

``` r
get_state(x, step_id, json = FALSE, extra_fields = NULL)
```

## Arguments

- x:

  Cohort object.

- step_id:

  If provided, the selected step state is returned.

- json:

  If TRUE, return state in JSON format.

- extra_fields:

  Names of extra fields included in filter to be added to state.

## Value

List object of character string being the list convertion to JSON
format.

## See also

[cohort-methods](https://r-world-devs.github.io/cohortBuilder/reference/cohort-methods.md)
