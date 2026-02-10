# Return reproducible data filtering code.

Return reproducible data filtering code.

## Usage

``` r
code(
  x,
  include_source = TRUE,
  include_methods = c(".pre_filtering", ".post_filtering", ".run_binding"),
  include_action = c("pre_filtering", "post_filtering", "run_binding"),
  modifier = .repro_code_tweak,
  mark_step = TRUE,
  ...
)
```

## Arguments

- x:

  Cohort object.

- include_source:

  If \`TRUE\` source generating code will be included.

- include_methods:

  Which methods definition should be included in the result.

- include_action:

  Which action should be returned in the result.
  \`pre_filtering\`/\`.post_filtering\` - to include data transformation
  before/after filtering. s\`run_binding\` - data binding
  transformation.

- modifier:

  A function taking data frame (storing reproducible code metadata) as
  an argument, and returning data frame with \`expr\` column which is
  then combined into a single expression (final result of \`get_code\`).
  See
  [.repro_code_tweak](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md).

- mark_step:

  Include information which filtering step is performed.

- ...:

  Other parameters passed to
  [tidy_source](https://rdrr.io/pkg/formatR/man/tidy_source.html).

## Value

[tidy_source](https://rdrr.io/pkg/formatR/man/tidy_source.html) output
storing reproducible code for generating final step data.

## See also

[cohort-methods](https://r-world-devs.github.io/cohortBuilder/reference/cohort-methods.md)
