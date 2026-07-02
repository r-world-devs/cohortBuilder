# Generate a deterministic filter ID from dataset and variable names.

Generate a deterministic filter ID from dataset and variable names.

## Usage

``` r
.default_filter_id(dataset, variables, suffix = NULL)
```

## Arguments

- dataset:

  Dataset name.

- variables:

  Character vector of variable names.

- suffix:

  Optional suffix (e.g. \`"md"\`, \`"q"\`).

## Value

A single character string suitable for use as a filter ID.
