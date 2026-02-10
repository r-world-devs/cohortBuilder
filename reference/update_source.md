# Update source in Cohort object.

Update source in Cohort object.

## Usage

``` r
update_source(x, source, keep_steps = !has_steps(source), run_flow = FALSE)
```

## Arguments

- x:

  Cohort object.

- source:

  Source object to be updated in Cohort.

- keep_steps:

  If \`TRUE\`, steps definition remain unchanged when updating source.
  If \`FALSE\` steps configuration is deleted. If vector of type
  integer, specified steps will remain.

- run_flow:

  If \`TRUE\`, data flow is run after the source is updated.

## Value

The \`Cohort\` class object with updated \`Source\` definition.

## See also

[managing-cohort](https://r-world-devs.github.io/cohortBuilder/reference/managing-cohort.md)
