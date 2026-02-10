# Trigger data calculations.

Trigger data calculations.

## Usage

``` r
run(x, min_step_id, step_id)
```

## Arguments

- x:

  Cohort object.

- min_step_id:

  Step id starting from the calculation will be started. Used only when
  \`step_id\` is missing.

- step_id:

  Id of the step for which to run data calculation.

## Value

The object of class \`Cohort\` having up-to-date data based on the
Cohort state.

## See also

[managing-cohort](https://r-world-devs.github.io/cohortBuilder/reference/managing-cohort.md)
