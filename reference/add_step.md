# Add filtering step definition

Add filtering step definition

## Usage

``` r
add_step(x, step, ...)

# S3 method for class 'Cohort'
add_step(
  x,
  step,
  run_flow = FALSE,
  hook = list(pre = get_hook("pre_add_step_hook"), post = get_hook("post_add_step_hook")),
  ...
)

# S3 method for class 'Source'
add_step(x, step, ...)
```

## Arguments

- x:

  An object to add step to.

- step:

  Step definition created with
  [step](https://r-world-devs.github.io/cohortBuilder/reference/step.md).

- ...:

  Other parameters passed to specific S3 method.

- run_flow:

  If \`TRUE\`, data flow is run after the step is added.

- hook:

  List of hooks describing methods to run before/after the step is
  added. See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

## Value

Method dependent object (i.e. \`Cohort\` or \`Source\`) having new step
added.

## See also

[managing-cohort](https://r-world-devs.github.io/cohortBuilder/reference/managing-cohort.md),
[managing-source](https://r-world-devs.github.io/cohortBuilder/reference/managing-source.md)
