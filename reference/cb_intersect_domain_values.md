# Intersect two domain values for a filter type

Combines two already-computed domain values (not value-vs-domain) into
their intersection, dispatching on filter type. Used by
\`"filter"\`-mode domain propagation when the same logical filter
appears in more than one upstream step and its effective domains must be
combined. Unlike \[cb_intersect_domain()\], this never emits trimming
messages.

## Usage

``` r
cb_intersect_domain_values(filter, ...)
```

## Arguments

- filter:

  S7 filter object (used for type dispatch only).

- ...:

  Additional arguments. Methods receive two domain values \`a\` and
  \`b\` to intersect (either may be \`NULL\`).

## Value

The intersected domain value, or \`NULL\`.
