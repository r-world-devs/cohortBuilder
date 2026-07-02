# Source compatibility methods.

List of methods that allow compatibility of different source types. Most
of the methods should be defined in order to make new source layer
functioning. See 'Details' section for more information.

## Usage

``` r
.init_step(source, ...)

# Default S3 method
.init_step(source, ...)

.collect_data(source, data_object)

# Default S3 method
.collect_data(source, data_object)

.get_stats(source, data_object)

# Default S3 method
.get_stats(source, data_object)

.pre_filtering(source, data_object, step_id)

.post_filtering(source, data_object, step_id)

.post_binding(source, data_object, step_id)

.repro_code_tweak(source, code_data)

# Default S3 method
.pre_filtering(source, data_object, step_id)

# Default S3 method
.post_filtering(source, data_object, step_id)

# Default S3 method
.post_binding(source, data_object, step_id)

.get_attrition_label(source, step_id, step_filters, ...)

# Default S3 method
.get_attrition_label(source, step_id, step_filters, ...)

.get_attrition_count(source, data_stats, ...)

# Default S3 method
.get_attrition_count(source, data_stats, ...)

.run_binding(source, ...)

# Default S3 method
.run_binding(source, binding_key, data_object_pre, data_object_post, ...)

# S3 method for class 'tblist'
.init_step(source, ...)

# S3 method for class 'tblist'
.collect_data(source, data_object)

# S3 method for class 'tblist'
.get_stats(source, data_object)
```

## Arguments

- source:

  Source object.

- ...:

  Other parameters passed to specific method.

- data_object:

  Object that allows source data access. \`data_object\` is the result
  of \`.init_step\` method (or object of the same structure).

- step_id:

  Name of the step visible in resulting plot.

- code_data:

  Data frame storing \`type\`, \`expr\` and filter or step related
  columns.

- step_filters:

  List of step filters.

- data_stats:

  Data frame presenting statistics for each filtering step.

- binding_key:

  Binding key describing currently processed relation.

- data_object_pre:

  Object storing unfiltered data in the current step (previous step
  result).

- data_object_post:

  Object storing current data (including active filtering and previously
  done bindings).

## Value

Depends on specific method. See \`vignette("custom-extensions")\` for
more details.

## Details

The package is designed to make the functionality work with multiple
data sources. Data source can be based for example on list of tables,
connection to database schema or API service that allows to access and
operate on data. In order to make new source type layer functioning, the
following list of methods should be defined:

- `.init_source` - Defines how to extract data object from source. Each
  filtering step assumes to be operating on resulting data object
  (further named data_object) and returns object of the same type and
  structure.

- `.collect_data` - Defines how to collect data (into R memory) from
  \`data_object\`.

- `.get_stats` - Defines what \`data_object\` statistics should be
  calculated and how. When provided the stats can be extracted using
  [stat](https://r-world-devs.github.io/cohortBuilder/reference/stat.md).

- `.pre_filtering` - (optional) Defines what operation on
  \`data_object\` should be performed before applying filtering in the
  step.

- `.post_filtering` - (optional) Defines what operation on
  \`data_object\` should be performed after applying filtering in the
  step (before running binding).

- `.post_binding` - (optional) Defines what operation on \`data_object\`
  should be performed after applying binding in the step.

- `.run_binding` - (optional) Defines how to handle post filtering data
  binding. See more about binding keys at
  [binding-keys](https://r-world-devs.github.io/cohortBuilder/reference/binding-keys.md).

- `.get_attrition_count and .get_attrition_label` - Methods defining how
  to get statistics and labels for attrition plot.

- `.repro_code_tweak` - (optional) Default method passed as a
  \`modifier\` argument of
  [code](https://r-world-devs.github.io/cohortBuilder/reference/code.md)
  function. Aims to modify reproducible code into the final format.

Except from the above methods, you may extend the existing or new source
with providing custom filtering methods. See
\`vignette("custom-filters")\`. In order to see more details about how
to implement custom source check \`vignette("custom-extensions")\`.
