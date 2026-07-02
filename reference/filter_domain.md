# Get a filter's domain

Returns the declared domain (universe of valid values) attached to a
filter, or \`NULL\` when no domain is set. This is a thin accessor over
the \`domain\` property, intended for downstream consumers (e.g. GUIs)
that render filter inputs from the domain without reaching into S7
internals.

## Usage

``` r
filter_domain(filter)
```

## Arguments

- filter:

  S7 filter object.

## Value

The filter's domain, or \`NULL\` when unset. Structure depends on the
filter type (e.g. a character vector for discrete filters, a two-element
vector for range filters).

## See also

\[filter_effective_value()\], \[cb_intersect_domain()\]
