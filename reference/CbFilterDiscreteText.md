# Discrete text filter class

Filters data by matching a variable against comma-separated text values.

## Usage

``` r
CbFilterDiscreteText(
  id = NULL,
  name = NULL,
  variable,
  value = NA,
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

- variable:

  Column name to filter on.

- value:

  Values to keep. \`NA\` means no filtering.

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
