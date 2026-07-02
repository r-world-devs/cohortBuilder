# Create new 'Cohort' object

Cohort object is designed to make operations on source data possible.

## Usage

``` r
cohort(
  source,
  ...,
  run_flow = FALSE,
  compute_stats = TRUE,
  propagate_domains = c("none", "filter", "stats", "data"),
  hook = list(pre = get_hook("pre_cohort_hook"), post = get_hook("post_cohort_hook"))
)
```

## Arguments

- source:

  Source object created with
  [set_source](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md).

- ...:

  Steps definition (optional). Can be also defined as a sequence of
  filters - the filters will be added to the first step.

- run_flow:

  If \`TRUE\`, data flow is run after the operation is completed.

- compute_stats:

  If \`TRUE\` (default), filter and step statistics are computed and
  stored after each step. Set to \`FALSE\` for metadata-only operation.

- propagate_domains:

  Domain propagation mode between steps: \`"none"\` (default),
  \`"filter"\` (from previous step filter values), \`"stats"\` (from
  stored statistics; requires \`compute_stats = TRUE\`), or \`"data"\`
  (scan filtered data; the stats-free equivalent).

- hook:

  List of hooks describing methods before/after the Cohort is created.
  See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

## Value

The object of class \`Cohort\`.
