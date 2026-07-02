# Create Cohort source

Source is an object storing information about data source such as source
type, primary keys and relations between stored data.

## Usage

``` r
set_source(
  dtconn,
  ...,
  primary_keys = NULL,
  binding_keys = NULL,
  source_code = NULL,
  description = NULL,
  available_filters = NULL,
  compute_meta_stats = getOption("cb.source_filters_meta_stats", TRUE)
)

# S3 method for class 'tblist'
set_source(
  dtconn,
  primary_keys = NULL,
  binding_keys = NULL,
  source_code = NULL,
  description = NULL,
  available_filters = NULL,
  compute_meta_stats = getOption("cb.source_filters_meta_stats", TRUE),
  ...
)
```

## Arguments

- dtconn:

  An object defining source data connection.

- ...:

  Source type specific parameters. Available in \`attributes\` list of
  resulting object.

- primary_keys:

  Definition of primary keys describing source data (if valid). When
  provided, affects the output of attrition data plot. See
  [primary_keys](https://r-world-devs.github.io/cohortBuilder/reference/primary_keys.md).

- binding_keys:

  Definition of binding keys describing relations in source data (if
  valid). When provided, affects post filtering data. See
  [binding-keys](https://r-world-devs.github.io/cohortBuilder/reference/binding-keys.md).

- source_code:

  Expression presenting low-level code for creating source. When
  provided, used as a part of reproducible code output.

- description:

  A named list storing the source objects description. Can be accessed
  with
  [description](https://r-world-devs.github.io/cohortBuilder/reference/description.md)
  Cohort method.

- available_filters:

  List of filter definitions available for the source. See
  [autofilter](https://r-world-devs.github.io/cohortBuilder/reference/autofilter.md)
  for generating them automatically.

- compute_meta_stats:

  Whether to pre-compute metadata statistics for the source
  \`available_filters\`. When \`FALSE\`, the computation is skipped and
  filter domains fall back to live computation. Defaults to the
  \`cb.source_filters_meta_stats\` option (\`TRUE\`).

## Value

R6 object of class inherited from \`dtconn\`.

## Examples

``` r
mtcars_source <- set_source(
  tblist(mtcars = mtcars),
  source_code = quote({
    source <- list(dtconn = list(datasets = mtcars))
  })
)
mtcars_source$attributes
#> list()
```
