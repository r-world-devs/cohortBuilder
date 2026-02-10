# Define custom filter.

Methods available for creating new filters easier.

## Usage

``` r
def_filter(
  type,
  id = .gen_id(),
  name = id,
  input_param = NULL,
  filter_data,
  get_stats,
  plot_data,
  get_params,
  get_data,
  get_defaults
)

new_filter(
  filter_type,
  source_type,
  input_param = "value",
  extra_params = "",
  file
)
```

## Arguments

- type:

  Filter type.

- id:

  Filter id.

- name:

  Filter name.

- input_param:

  Name of the parameter taking filtering value.

- filter_data:

  Function of \`data_object\` parameter defining filtering logic on
  Source data object.

- get_stats:

  Function of \`data_object\` and \`name\` parameters defining what and
  how data statistics should be calculated.

- plot_data:

  Function of \`data_object\` parameter defining how filter data should
  be plotted.

- get_params:

  Function of \`name\` parameter returning filter parameters (if names
  is skipped all the parameters are returned).

- get_data:

  Function of \`data_object\` returning filter related data.

- get_defaults:

  Function of \`data_object\` and \`cache_object\` parameters returning
  default \`input_param\` parameter value.

- filter_type:

  Type of new filter.

- source_type:

  Type of source for which filter should be defined.

- extra_params:

  Vector of extra parameters name that should be available for filter.

- file:

  File path where filter should be created.

## Value

A list of filter specific values and methods (\`def_filter\`) or no
value (\`new_filter\`).

## Details

\`def_filter\` designates list of parameters and methods required to
define new type of filter.

\`new_filter\` creates a new file with new filter definition template.

See vignettes("custom-filters") to learn how to create a custom filter.
