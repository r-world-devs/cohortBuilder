# Propagate domains between steps

Source-layer generic that recomputes the domains of a \*\*target
step\*\* from that step's parent (the previous step). Override to
implement dynamic domain narrowing.

## Usage

``` r
.propagate_domains(source, data_object, step_id, cohort, ...)

# Default S3 method
.propagate_domains(source, data_object, step_id, cohort, ...)

# S3 method for class 'tblist'
.propagate_domains(source, data_object, step_id, cohort, mode, ...)
```

## Arguments

- source:

  Source object.

- data_object:

  Filtered data of the \*\*parent\*\* step (the input to the target
  step), used by \`mode = "data"\`.

- step_id:

  Target step id whose domains should be recomputed.

- cohort:

  Cohort object (for accessing the parent step's filters and stored
  statistics).

- ...:

  Additional arguments.

- mode:

  Propagation mode: \`"filter"\` (from upstream filter values, no data
  access), \`"stats"\` (from the parent's post-step stored stats), or
  \`"data"\` (scan the parent's filtered data directly).

## Details

The default is a no-op. Only called when \`propagate_domains != "none"\`
in the Cohort.

Contract: \`step_id = N\` recomputes the domains of step \`N\`'s filters
using step \`N - 1\` as the source of truth. It is a no-op when \`N\`
has no parent (i.e. \`N == "1"\`) or when the target step is absent.
