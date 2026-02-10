# Update filter definition

Update filter definition

## Usage

``` r
update_filter(x, step_id, filter_id, ...)

# S3 method for class 'Cohort'
update_filter(x, step_id, filter_id, ..., run_flow = FALSE)

# S3 method for class 'Source'
update_filter(x, step_id, filter_id, ...)
```

## Arguments

- x:

  An object in which the filter should be updated.

- step_id:

  Id of the step where filter is defined.

- filter_id:

  Id of the filter to be updated.

- ...:

  Filter parameters that should be updated.

- run_flow:

  If \`TRUE\`, data flow is run after the filter is updated.

## Value

Method dependent object (i.e. \`Cohort\` or \`Source\`) having selected
filter updated.

## See also

[managing-cohort](https://r-world-devs.github.io/cohortBuilder/reference/managing-cohort.md),
[managing-source](https://r-world-devs.github.io/cohortBuilder/reference/managing-source.md)
