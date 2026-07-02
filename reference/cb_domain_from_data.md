# Extract domain from data

Derives a domain (set of valid values) directly from the data object.
Uses dual dispatch on filter type and source type. Custom filter types
should implement S7 methods for this generic. The default method returns
\`NULL\` (no domain).

## Usage

``` r
cb_domain_from_data(filter, source, ...)
```

## Arguments

- filter:

  S7 filter object.

- source:

  Source object.

- ...:

  Additional arguments. Methods receive a \`data_object\` (the data to
  extract the domain from) here.

## Value

Domain value appropriate for the filter type, or \`NULL\`.
