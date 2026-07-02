# Multi-discrete filter class

Filters data by matching multiple variables against sets of discrete
values.

## Usage

``` r
CbFilterMultiDiscrete(
  id = NULL,
  name = NULL,
  values,
  variables,
  dataset,
  keep_na = TRUE,
  description = NULL,
  domain = NULL,
  active = getOption("cb_active_filter", default = TRUE),
  ...
)
```

## Arguments

- id:

  Filter identifier.

- name:

  Filter display name (defaults to \`id\`).

- values:

  Named list of values to filter by, keyed by variable name.

- variables:

  Vector of column names to filter on.

- dataset:

  Dataset name.

- keep_na:

  If \`TRUE\`, NA values are retained.

- description:

  Optional description.

- domain:

  Optional filter domain (the set of allowed values). \`NULL\` means the
  domain is derived from the data.

- active:

  If \`FALSE\`, filter is skipped.

- ...:

  Extra parameters.
