# R6 class representing a data source

R6 class representing a data source

R6 class representing a data source

## Details

Source is an object storing information about data source such as source
type, primary keys and relations between stored data.

## Public fields

- `dtconn`:

  Data connection object the Source if based on.

- `dtvalue`:

  Evaluated data connection value used for computing stats.

- `meta_stats`:

  Computed metadata statistics for available filters.

- `compute_meta_stats`:

  Whether metadata statistics for available filters are pre-computed.

- `description`:

  Source object description list.

- `attributes`:

  Extra source parameters passed when source is defined.

- `options`:

  Extra configuration options.

- `binding_keys`:

  Source data relations expressed as
  [binding-keys](https://r-world-devs.github.io/cohortBuilder/reference/binding-keys.md).

- `primary_keys`:

  Source data primary keys expressed as
  [primary_keys](https://r-world-devs.github.io/cohortBuilder/reference/primary_keys.md).

- `source_code`:

  An expression which allows to recreate basic source structure.

## Active bindings

- `available_filters`:

  List of filter definitions available for the source.

## Methods

### Public methods

- [`Source$new()`](#method-Source-new)

- [`Source$get()`](#method-Source-get)

- [`Source$get_steps()`](#method-Source-get_steps)

- [`Source$add_step()`](#method-Source-add_step)

- [`Source$rm_step()`](#method-Source-rm_step)

- [`Source$add_filter()`](#method-Source-add_filter)

- [`Source$rm_filter()`](#method-Source-rm_filter)

- [`Source$update_filter()`](#method-Source-update_filter)

- [`Source$calc_meta_stats()`](#method-Source-calc_meta_stats)

- [`Source$clone()`](#method-Source-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new \`Source\` object.

#### Usage

    Source$new(
      dtconn,
      ...,
      primary_keys = NULL,
      binding_keys = NULL,
      source_code = NULL,
      description = NULL,
      available_filters = NULL,
      compute_meta_stats = getOption("cb.source_filters_meta_stats", TRUE),
      options = list(display_binding = TRUE)
    )

#### Arguments

- `dtconn`:

  An object defining source data connection.

- `...`:

  Extra Source parameters. Stored within \`attributes\` field.

- `primary_keys`:

  Definition of data \`primary_keys\`, if appropriate. See
  [primary_keys](https://r-world-devs.github.io/cohortBuilder/reference/primary_keys.md).

- `binding_keys`:

  Definition of relations between data, if appropriate. See
  [binding-keys](https://r-world-devs.github.io/cohortBuilder/reference/binding-keys.md).

- `source_code`:

  A quote object that allows to recreate basic source structure. Used as
  a part of reproducible code output, see
  [code](https://r-world-devs.github.io/cohortBuilder/reference/code.md).

- `description`:

  A named list storing the source objects description. Can be accessed
  with
  [description](https://r-world-devs.github.io/cohortBuilder/reference/description.md)
  Cohort method.

- `available_filters`:

  List of filter definitions available for the source.

- `compute_meta_stats`:

  Whether to pre-compute metadata statistics for \`available_filters\`.
  When \`FALSE\`, \`meta_stats\` are skipped (and filter domains fall
  back to live computation). Defaults to the
  \`cb.source_filters_meta_stats\` option (\`TRUE\`).

- `options`:

  List of options affecting methods output. Currently supported only
  \`display_binding\` specifying whether reproducible code should
  include bindings definition.

#### Returns

A new \`Source\` object of class \`Source\` (and \`dtconn\` object class
appended).

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get selected \`Source\` object \`attribute\`.

#### Usage

    Source$get(param)

#### Arguments

- `param`:

  Name of the attribute.

------------------------------------------------------------------------

### Method `get_steps()`

Returns filtering steps definition, if defined for \`Source\`.

#### Usage

    Source$get_steps()

------------------------------------------------------------------------

### Method [`add_step()`](https://r-world-devs.github.io/cohortBuilder/reference/add_step.md)

Add filtering step definition.

#### Usage

    Source$add_step(step)

#### Arguments

- `step`:

  Step definition created with
  [step](https://r-world-devs.github.io/cohortBuilder/reference/step.md).

------------------------------------------------------------------------

### Method [`rm_step()`](https://r-world-devs.github.io/cohortBuilder/reference/rm_step.md)

Remove filtering step definition.

#### Usage

    Source$rm_step(step_id)

#### Arguments

- `step_id`:

  Id of the step to be removed.

------------------------------------------------------------------------

### Method [`add_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/add_filter.md)

Add filter definition to selected step.

#### Usage

    Source$add_filter(filter, step_id)

#### Arguments

- `filter`:

  Filter definition created with
  [filter](https://r-world-devs.github.io/cohortBuilder/reference/filter.md).

- `step_id`:

  Id of the step to include the filter to. If skipped the last step is
  used.

------------------------------------------------------------------------

### Method [`rm_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/rm_filter.md)

Remove filter definition from selected step.

#### Usage

    Source$rm_filter(step_id, filter_id)

#### Arguments

- `step_id`:

  Id of the step where filter is defined.

- `filter_id`:

  Id of the filter to be removed.

------------------------------------------------------------------------

### Method [`update_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/update_filter.md)

Update filter definition.

#### Usage

    Source$update_filter(step_id, filter_id, ...)

#### Arguments

- `step_id`:

  Id of the step where filter is defined.

- `filter_id`:

  Id of the filter to be updated.

- `...`:

  Parameters with its new values.

------------------------------------------------------------------------

### Method `calc_meta_stats()`

Calculate metadata statistics for available filters.

#### Usage

    Source$calc_meta_stats()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Source$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
