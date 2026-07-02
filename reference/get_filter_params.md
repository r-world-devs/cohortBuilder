# Get filter parameters as a list

Extracts all user-facing properties from an S7 filter object.

## Usage

``` r
get_filter_params(filter, name)
```

## Arguments

- filter:

  S7 filter object.

- name:

  Optional parameter name to retrieve a single value.

## Value

Named list of filter parameters, or a single value if \`name\` is given.
Properties stored in \`filter@private\` are always excluded.
