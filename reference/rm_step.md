# Remove filtering step definition

Remove filtering step definition

## Usage

``` r
rm_step(x, step_id, ...)

# S3 method for class 'Cohort'
rm_step(
  x,
  step_id,
  run_flow = FALSE,
  hook = list(pre = get_hook("pre_rm_step_hook"), post = get_hook("post_rm_step_hook")),
  ...
)

# S3 method for class 'Source'
rm_step(x, step_id, ...)
```

## Arguments

- x:

  An object from which step should be removed.

- step_id:

  Id of the step to remove.

- ...:

  Other parameters passed to specific S3 method.

- run_flow:

  If \`TRUE\`, data flow is run after the step is removed.

- hook:

  List of hooks describing methods before/after the Cohort is created.
  See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

## Value

Method dependent object (i.e. \`Cohort\` or \`Source\`) having selected
step removed.

## See also

[managing-cohort](https://r-world-devs.github.io/cohortBuilder/reference/managing-cohort.md),
[managing-source](https://r-world-devs.github.io/cohortBuilder/reference/managing-source.md)
