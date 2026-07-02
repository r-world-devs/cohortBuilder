# Define Cohort filter

Creates an S7 filter object of the specified type.

## Usage

``` r
filter(type, ...)
```

## Arguments

- type:

  Type of filter to use (e.g., "discrete", "range", "date_range").

- ...:

  Filter type-specific parameters.

## Value

An S7 filter object inheriting from \`CbFilter\`.
