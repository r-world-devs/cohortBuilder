# Get the variables a filter operates on

Returns the column name(s) a filter is bound to, regardless of whether
the filter stores them in a single-variable property (\`variable\`) or a
multi-variable property (\`variables\`).

## Usage

``` r
filter_variables(filter)
```

## Arguments

- filter:

  S7 filter object.

## Value

Character vector of variable (column) names. Empty when the filter
declares no variables.
