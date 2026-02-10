# Show source data or filter description

If defined allows to check the provided description related to source
data or configured filters.

## Usage

``` r
description(
  x,
  field,
  step_id,
  filter_id,
  modifier = getOption("cb_help_modifier", default = function(x) x)
)
```

## Arguments

- x:

  Cohort object.

- field:

  Name of the source description field provided as \`description\`
  argument to
  [set_source](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md).
  If missing, \`step_id\` and \`filter_id\` are used to return filter
  description.

- step_id:

  Id of the filter step to return description of.

- filter_id:

  Id of the filter to return description of.

- modifier:

  A function taking the description as argument. The function can be
  used to modify its argument (convert to html, display in browser
  etc.).

## Value

Any object (or its subset) attached to Source of filter via description
argument.

## See also

[cohort-methods](https://r-world-devs.github.io/cohortBuilder/reference/cohort-methods.md)
