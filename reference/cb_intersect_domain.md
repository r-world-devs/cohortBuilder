# Get effective filter value after domain intersection

Returns the filter's value intersected with its domain. When value is
\`NA\` and domain is set, returns the domain as the effective value.

## Usage

``` r
cb_intersect_domain(filter, ...)
```

## Arguments

- filter:

  S7 filter object.

- ...:

  Additional arguments passed to methods.

## Value

The effective value for filtering.

## Details

Custom filter types should implement an S7 method for this generic. The
default method returns the raw value with no domain logic.
