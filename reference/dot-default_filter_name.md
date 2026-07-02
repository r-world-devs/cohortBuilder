# Generate a default filter display name from its variables.

Single-variable filters are named after their variable. Multi-variable
filters get a summarised name listing the first two variables, the count
of remaining variables, and the filter type, e.g. \`"age sex + 2 vars
multi_discrete"\`.

## Usage

``` r
.default_filter_name(variables, type)
```

## Arguments

- variables:

  Character vector of variable names.

- type:

  Filter type string (e.g. \`"multi_discrete"\`, \`"query"\`).

## Value

A single character string suitable for use as a filter name.
