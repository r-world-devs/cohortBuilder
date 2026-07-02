# Add filter definition

Add filter definition

## Usage

``` r
add_filter(x, filter, step_id, ...)

# S3 method for class 'Cohort'
add_filter(
  x,
  filter,
  step_id,
  run_flow = FALSE,
  hook = list(pre = get_hook("pre_add_filter_hook"), post =
    get_hook("post_add_filter_hook")),
  ...
)

# S3 method for class 'Source'
add_filter(x, filter, step_id, ...)
```

## Arguments

- x:

  An object to add filter to.

- filter:

  Filter definition created with
  [filter](https://r-world-devs.github.io/cohortBuilder/reference/filter.md).

- step_id:

  Id of the step to add the filter to. If missing, filter is added to
  the last step.

- ...:

  Other parameters passed to specific S3 method.

- run_flow:

  If \`TRUE\`, data flow is run after the filter is added.

- hook:

  List of hooks describing methods to run before/after the filter is
  added. See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

## Value

Method dependent object (i.e. \`Cohort\` or \`Source\`) having filter
added in selected step.

## See also

[managing-cohort](https://r-world-devs.github.io/cohortBuilder/reference/managing-cohort.md),
[managing-source](https://r-world-devs.github.io/cohortBuilder/reference/managing-source.md)
