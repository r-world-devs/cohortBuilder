# Query filter class

Filters data using a queryBuilder query object.

## Usage

``` r
CbFilterQuery(
  id = NULL,
  name = NULL,
  variables,
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

- variables:

  Vector of column names used in the query.

- value:

  Query object (from queryBuilder package). \`NA\` means no filtering.

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
