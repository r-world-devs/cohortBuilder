# Filter Source types methods

Filter Source types methods

## Usage

``` r
cb_filter.discrete(source, ...)

cb_filter.discrete_text(source, ...)

cb_filter.range(source, ...)

cb_filter.date_range(source, ...)

cb_filter.datetime_range(source, ...)

cb_filter.multi_discrete(source, ...)

cb_filter.query(source, ...)

# S3 method for class 'tblist'
cb_filter.discrete(
  source,
  type = "discrete",
  id = .gen_id(),
  name = id,
  variable,
  value = NA,
  dataset,
  keep_na = TRUE,
  ...,
  description = NULL,
  active = TRUE
)

# S3 method for class 'tblist'
cb_filter.discrete_text(
  source,
  type = "discrete_text",
  id = .gen_id(),
  name = id,
  variable,
  value = NA,
  dataset,
  ...,
  description = NULL,
  active = TRUE
)

# S3 method for class 'tblist'
cb_filter.range(
  source,
  type = "range",
  id = .gen_id(),
  name = id,
  variable,
  range = NA,
  dataset,
  keep_na = TRUE,
  ...,
  description = NULL,
  active = TRUE
)

# S3 method for class 'tblist'
cb_filter.date_range(
  source,
  type = "date_range",
  id = .gen_id(),
  name = id,
  variable,
  range = NA,
  dataset,
  keep_na = TRUE,
  ...,
  description = NULL,
  active = TRUE
)

# S3 method for class 'tblist'
cb_filter.datetime_range(
  source,
  type = "datetime_range",
  id = .gen_id(),
  name = id,
  variable,
  range = NA,
  dataset,
  keep_na = TRUE,
  ...,
  description = NULL,
  active = TRUE
)

# S3 method for class 'tblist'
cb_filter.multi_discrete(
  source,
  type = "multi_discrete",
  id = .gen_id(),
  name = id,
  values,
  variables,
  dataset,
  keep_na = TRUE,
  ...,
  description = NULL,
  active = TRUE
)

# S3 method for class 'tblist'
cb_filter.query(
  source,
  type = "query",
  id = .gen_id(),
  name = id,
  variables,
  value = NA,
  dataset,
  keep_na = TRUE,
  ...,
  description = NULL,
  active = TRUE
)
```

## Arguments

- source:

  Source object.

- ...:

  Source type specific parameters (or extra ones if not matching
  specific S3 method arguments).

- type:

  Character string defining filter type (having class of the same value
  as type).

- id:

  Id of the filter.

- name:

  Filter name.

- variable:

  Dataset variable used for filtering.

- value:

  Value(s) to be used for filtering.

- dataset:

  Dataset name to be used for filtering.

- keep_na:

  If \`TRUE\`, NA values are included.

- description:

  Filter description (optional).

- active:

  If FALSE filter will be skipped during Cohort filtering.

- range:

  Variable range to be applied in filtering.

- values:

  Named list of values to be applied in filtering. The names should
  relate to the ones included in \`variables\` parameter.

- variables:

  Dataset variables used for filtering.

## Value

List of filter-specific metadata and methods - result of evaluation of
\`cb_filter_constructor\` function on \`Source\` object.
