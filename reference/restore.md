# Restore Cohort object.

The method allows to restore Cohort object with provided configuration
state.

## Usage

``` r
restore(
  x,
  state,
  modifier = function(prev_state, state) state,
  run_flow = FALSE
)
```

## Arguments

- x:

  Cohort object.

- state:

  List or JSON string containing steps and filters configuration. See
  [get_state](https://r-world-devs.github.io/cohortBuilder/reference/get_state.md).

- modifier:

  Function two parameters combining the previous and provided state. The
  returned state is then restored.

- run_flow:

  If TRUE, filtering flow is applied when the operation is finished.

## Value

The \`Cohort\` class object having the state restored based on provided
config.

## See also

[cohort-methods](https://r-world-devs.github.io/cohortBuilder/reference/cohort-methods.md)
