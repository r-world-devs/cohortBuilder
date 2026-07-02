# Get a filter's effective value

Returns the value that should be used to pre-select the filter input,
accounting for the domain. This is the filter's value intersected with
its domain; when the value is unset (\`NA\`) and a domain is present,
the domain is returned as the effective value. Thin wrapper over
\[cb_intersect_domain()\] for use by GUIs.

## Usage

``` r
filter_effective_value(filter)
```

## Arguments

- filter:

  S7 filter object.

## Value

The effective value for the filter, suitable for pre-selecting an input.
Structure depends on the filter type.

## See also

\[filter_domain()\], \[cb_intersect_domain()\]
