# Create a description object

Helper for building structured description entries used in source
definition.

## Usage

``` r
describe(description, label = NULL, ...)
```

## Arguments

- description:

  A single string describing the field.

- label:

  Optional short, human-readable label for the field. When the field
  describes a variable, \[autofilter()\] reuses the label to fill the
  generated filter's \`name\`. \`NULL\` (default) leaves the name
  untouched.

- ...:

  Additional named parameters to include in the description object.

## Value

A named list with \`text\`, an optional \`label\`, and any extra
parameters.
