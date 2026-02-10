# Operator simplifying adding steps or filters to Cohort and Source objects

When called with filter or step object, runs add_filter and add_step
respectively.

## Usage

``` r
x %->% object
```

## Arguments

- x:

  Source or Cohort object. Otherwise works as a standard pipe operator.

- object:

  Filter or step to be added to \`x\`.

## Value

And object (\`Source\` or \`Cohort\`) having new filter of step added.
