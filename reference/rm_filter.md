# Remove filter definition

Remove filter definition

## Usage

``` r
rm_filter(x, step_id, filter_id, ...)

# S3 method for class 'Cohort'
rm_filter(x, step_id, filter_id, run_flow = FALSE, ...)

# S3 method for class 'Source'
rm_filter(x, step_id, filter_id, ...)
```

## Arguments

- x:

  An object from which filter should be removed.

- step_id:

  Id of the step from which filter should be removed.

- filter_id:

  Id of the filter to be removed.

- ...:

  Other parameters passed to specific S3 method.

- run_flow:

  If \`TRUE\`, data flow is run after the filter is removed.

## Value

Method dependent object (i.e. \`Cohort\` or \`Source\`) having selected
filter removed.

## See also

[managing-cohort](https://r-world-devs.github.io/cohortBuilder/reference/managing-cohort.md),
[managing-source](https://r-world-devs.github.io/cohortBuilder/reference/managing-source.md)
