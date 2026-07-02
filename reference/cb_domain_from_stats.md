# Extract domain from stored filter statistics

Derives a domain (set of valid values) from previously computed filter
statistics. Custom filter types should implement an S7 method for this
generic. The default method returns \`NULL\` (no domain).

## Usage

``` r
cb_domain_from_stats(filter, ...)
```

## Arguments

- filter:

  S7 filter object.

- ...:

  Additional arguments. Methods receive \`stats\`, the list of stored
  statistics for the filter.

## Value

Domain value appropriate for the filter type, or \`NULL\`.
