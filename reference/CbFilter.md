# Base class for all cohortBuilder filters

Base class for all cohortBuilder filters

## Usage

``` r
CbFilter(
  type = character(0),
  id = character(0),
  name = character(0),
  active = logical(0),
  description = NULL,
  domain = NULL,
  step_id = NULL,
  extra = list(),
  private = list()
)
```

## Arguments

- type:

  Filter type string.

- id:

  Filter identifier.

- name:

  Filter display name.

- active:

  Whether the filter is active.

- description:

  Optional filter description.

- domain:

  Optional domain constraining valid filter values. Structure depends on
  filter type: character vector for discrete, 2-length vector for range,
  named list for multi_discrete. When set, filter values are intersected
  with the domain. When value is unset (\`NA\`) and domain is provided,
  the domain serves as the effective value.

- step_id:

  Step identifier (set when filter is attached to a step).

- extra:

  Named list of extra parameters.

- private:

  Named list of internal parameters, not intended to be set directly by
  users.
