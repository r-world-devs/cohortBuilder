# Filter types

Filter types

## Usage

``` r
# S3 method for class 'discrete'
filter(
  type,
  id,
  name,
  ...,
  active = getOption("cb_active_filter", default = TRUE)
)

# S3 method for class 'discrete_text'
filter(
  type,
  id,
  name,
  ...,
  description = NULL,
  active = getOption("cb_active_filter", default = TRUE)
)

# S3 method for class 'range'
filter(
  type,
  id,
  name,
  ...,
  description = NULL,
  active = getOption("cb_active_filter", default = TRUE)
)

# S3 method for class 'date_range'
filter(
  type,
  id,
  name,
  ...,
  description = NULL,
  active = getOption("cb_active_filter", default = TRUE)
)

# S3 method for class 'datetime_range'
filter(
  type,
  id,
  name,
  ...,
  description = NULL,
  active = getOption("cb_active_filter", default = TRUE)
)

# S3 method for class 'multi_discrete'
filter(
  type,
  id,
  name,
  ...,
  description = NULL,
  active = getOption("cb_active_filter", default = TRUE)
)

# S3 method for class 'query'
filter(
  type,
  id,
  name,
  ...,
  active = getOption("cb_active_filter", default = TRUE)
)
```

## Arguments

- type:

  Character string defining filter type (having class of the same value
  as type).

- id:

  Id of the filter.

- name:

  Filter name.

- ...:

  Source specific parameters passed to filter (see
  [filter-source-types](https://r-world-devs.github.io/cohortBuilder/reference/filter-source-types.md)).

- active:

  If FALSE filter will be skipped during Cohort filtering.

- description:

  Filter description object. Preferable a character value.

## Value

A function of class \`cb_filter_constructor\`.
