# R6 class representing Cohort object.

R6 class representing Cohort object.

R6 class representing Cohort object.

## Details

Cohort object is designed to make operations on source data possible.

## Public fields

- `attributes`:

  List of Cohort attributes defined while creating a new Cohort object.

## Methods

### Public methods

- [`Cohort$new()`](#method-Cohort-new)

- [`Cohort$add_source()`](#method-Cohort-add_source)

- [`Cohort$update_source()`](#method-Cohort-update_source)

- [`Cohort$get_source()`](#method-Cohort-get_source)

- [`Cohort$add_step()`](#method-Cohort-add_step)

- [`Cohort$copy_step()`](#method-Cohort-copy_step)

- [`Cohort$remove_step()`](#method-Cohort-remove_step)

- [`Cohort$add_filter()`](#method-Cohort-add_filter)

- [`Cohort$remove_filter()`](#method-Cohort-remove_filter)

- [`Cohort$update_filter()`](#method-Cohort-update_filter)

- [`Cohort$clear_filter()`](#method-Cohort-clear_filter)

- [`Cohort$clear_step()`](#method-Cohort-clear_step)

- [`Cohort$sum_up_state()`](#method-Cohort-sum_up_state)

- [`Cohort$get_state()`](#method-Cohort-get_state)

- [`Cohort$restore()`](#method-Cohort-restore)

- [`Cohort$get_data()`](#method-Cohort-get_data)

- [`Cohort$plot_data()`](#method-Cohort-plot_data)

- [`Cohort$show_attrition()`](#method-Cohort-show_attrition)

- [`Cohort$get_stats()`](#method-Cohort-get_stats)

- [`Cohort$show_help()`](#method-Cohort-show_help)

- [`Cohort$get_code()`](#method-Cohort-get_code)

- [`Cohort$run_flow()`](#method-Cohort-run_flow)

- [`Cohort$run_step()`](#method-Cohort-run_step)

- [`Cohort$bind_data()`](#method-Cohort-bind_data)

- [`Cohort$describe_state()`](#method-Cohort-describe_state)

- [`Cohort$get_step()`](#method-Cohort-get_step)

- [`Cohort$get_filter()`](#method-Cohort-get_filter)

- [`Cohort$update_cache()`](#method-Cohort-update_cache)

- [`Cohort$get_cache()`](#method-Cohort-get_cache)

- [`Cohort$list_active_filters()`](#method-Cohort-list_active_filters)

- [`Cohort$last_step_id()`](#method-Cohort-last_step_id)

- [`Cohort$modify()`](#method-Cohort-modify)

- [`Cohort$clone()`](#method-Cohort-clone)

------------------------------------------------------------------------

### Method `new()`

Create Cohort object.

#### Usage

    Cohort$new(
      source,
      ...,
      run_flow = FALSE,
      hook = list(pre = get_hook("pre_cohort_hook"), post = get_hook("post_cohort_hook"))
    )

#### Arguments

- `source`:

  Source object created with
  [set_source](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md).

- `...`:

  Steps definition (optional). Can be also defined as a sequence of
  filters - the filters will be added to the first step.

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

- `hook`:

  List of hooks describing methods before/after the Cohort is created.
  See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

#### Returns

The object of class \`Cohort\`.

------------------------------------------------------------------------

### Method [`add_source()`](https://r-world-devs.github.io/cohortBuilder/reference/add_source.md)

Add Source to Cohort object.

#### Usage

    Cohort$add_source(source)

#### Arguments

- `source`:

  Source object created with
  [set_source](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md).

------------------------------------------------------------------------

### Method [`update_source()`](https://r-world-devs.github.io/cohortBuilder/reference/update_source.md)

Update Source in the Cohort object.

#### Usage

    Cohort$update_source(
      source,
      keep_steps = !has_steps(source),
      run_flow = FALSE,
      hook = list(pre = get_hook("pre_update_source_hook"), post =
        get_hook("post_update_source_hook"))
    )

#### Arguments

- `source`:

  Source object created with
  [set_source](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md).

- `keep_steps`:

  If \`TRUE\`, steps definition remains unchanged when updating source.
  If \`FALSE\` steps configuration is deleted. If vector of type
  integer, specified steps will remain.

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

- `hook`:

  List of hooks describing methods before/after the Cohort is created.
  See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

------------------------------------------------------------------------

### Method `get_source()`

Return Source object attached to Cohort.

#### Usage

    Cohort$get_source()

------------------------------------------------------------------------

### Method [`add_step()`](https://r-world-devs.github.io/cohortBuilder/reference/add_step.md)

Add filtering step definition

#### Usage

    Cohort$add_step(
      step,
      run_flow = FALSE,
      hook = list(pre = get_hook("pre_add_step_hook"), post = get_hook("post_add_step_hook"))
    )

#### Arguments

- `step`:

  Step definition created with
  [step](https://r-world-devs.github.io/cohortBuilder/reference/step.md).

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

- `hook`:

  List of hooks describing methods before/after the Cohort is created.
  See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

------------------------------------------------------------------------

### Method `copy_step()`

Copy selected step.

#### Usage

    Cohort$copy_step(step_id, filters, run_flow = FALSE)

#### Arguments

- `step_id`:

  Id of the step to be copied. If missing the last step is taken. The
  copied step is added as the last one in the Cohort.

- `filters`:

  List of Source-evaluated filters to copy to new step.

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

------------------------------------------------------------------------

### Method `remove_step()`

Remove filtering step definition

#### Usage

    Cohort$remove_step(
      step_id,
      run_flow = FALSE,
      hook = list(pre = get_hook("pre_rm_step_hook"), post = get_hook("post_rm_step_hook"))
    )

#### Arguments

- `step_id`:

  Id of the step to remove.

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

- `hook`:

  List of hooks describing methods before/after the Cohort is created.
  See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

------------------------------------------------------------------------

### Method [`add_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/add_filter.md)

Add filter definition

#### Usage

    Cohort$add_filter(filter, step_id, run_flow = FALSE)

#### Arguments

- `filter`:

  Filter definition created with
  [filter](https://r-world-devs.github.io/cohortBuilder/reference/filter.md).

- `step_id`:

  Id of the step to add the filter to. If missing, filter is added to
  the last step.

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

------------------------------------------------------------------------

### Method `remove_filter()`

Remove filter definition

#### Usage

    Cohort$remove_filter(step_id, filter_id, run_flow = FALSE)

#### Arguments

- `step_id`:

  Id of the step from which filter should be removed.

- `filter_id`:

  Id of the filter to be removed.

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

------------------------------------------------------------------------

### Method [`update_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/update_filter.md)

Update filter definition

#### Usage

    Cohort$update_filter(step_id, filter_id, ..., active, run_flow = FALSE)

#### Arguments

- `step_id`:

  Id of the step where filter is defined.

- `filter_id`:

  Id of the filter to be updated.

- `...`:

  Filter parameters that should be updated.

- `active`:

  Mark filter as active (\`TRUE\`) or inactive (\`FALSE\`).

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

------------------------------------------------------------------------

### Method `clear_filter()`

Reset filter to its default values.

#### Usage

    Cohort$clear_filter(step_id, filter_id, run_flow = FALSE)

#### Arguments

- `step_id`:

  Id of the step where filter is defined.

- `filter_id`:

  Id of the filter which should be cleared.

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

------------------------------------------------------------------------

### Method `clear_step()`

Reset all filters included in selected step.

#### Usage

    Cohort$clear_step(step_id, run_flow = FALSE)

#### Arguments

- `step_id`:

  Id of the step where filters should be cleared.

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

------------------------------------------------------------------------

### Method `sum_up_state()`

Sum up Cohort configuration - Source, steps definition and evaluated
data.

#### Usage

    Cohort$sum_up_state()

------------------------------------------------------------------------

### Method [`get_state()`](https://r-world-devs.github.io/cohortBuilder/reference/get_state.md)

Get Cohort configuration state.

#### Usage

    Cohort$get_state(step_id, json = FALSE, extra_fields = NULL)

#### Arguments

- `step_id`:

  If provided, the selected step state is returned.

- `json`:

  If TRUE, return state in JSON format.

- `extra_fields`:

  Names of extra fields included in filter to be added to state. Restore
  Cohort configuration.

------------------------------------------------------------------------

### Method [`restore()`](https://r-world-devs.github.io/cohortBuilder/reference/restore.md)

#### Usage

    Cohort$restore(
      state,
      modifier = function(prev_state, state) state,
      run_flow = FALSE,
      hook = list(pre = get_hook("pre_restore_hook"), post = get_hook("post_restore_hook"))
    )

#### Arguments

- `state`:

  List or JSON string containing steps and filters configuration.

- `modifier`:

  Function two parameters combining the previous and provided state. The
  returned state is then restored.

- `run_flow`:

  If \`TRUE\`, data flow is run after the operation is completed.

- `hook`:

  List of hooks describing methods before/after the Cohort is created.
  See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

------------------------------------------------------------------------

### Method [`get_data()`](https://r-world-devs.github.io/cohortBuilder/reference/get_data.md)

Get step related data

#### Usage

    Cohort$get_data(step_id, state = "post", collect = TRUE)

#### Arguments

- `step_id`:

  Id of the step from which to source data.

- `state`:

  Return data before ("pre") or after ("post") step filtering?

- `collect`:

  Return raw data source (\`FALSE\`) object or collected (to R memory)
  data (\`TRUE\`).

------------------------------------------------------------------------

### Method [`plot_data()`](https://r-world-devs.github.io/cohortBuilder/reference/plot_data.md)

Plot filter specific data summary.

#### Usage

    Cohort$plot_data(step_id, filter_id, ..., state = "post")

#### Arguments

- `step_id`:

  Id of the step where filter is defined.

- `filter_id`:

  Id of the filter for which the plot should be returned

- `...`:

  Another parameters passed to filter specific method.

- `state`:

  Generate plot on data before ("pre") or after ("post") step filtering?

------------------------------------------------------------------------

### Method `show_attrition()`

Show attrition plot.

#### Usage

    Cohort$show_attrition(..., percent = FALSE)

#### Arguments

- `...`:

  Source specific parameters required to generate attrition.

- `percent`:

  Should attrition changes be presented with percentage values.

------------------------------------------------------------------------

### Method `get_stats()`

Get Cohort related statistics.

#### Usage

    Cohort$get_stats(step_id, filter_id, ..., state = "post")

#### Arguments

- `step_id`:

  When \`filter_id\` specified, \`step_id\` precises from which step the
  filter comes from. Otherwise data from specified step is used to
  calculate required statistics.

- `filter_id`:

  If not missing, filter related data statistics are returned.

- `...`:

  Specific parameters passed to filter related method.

- `state`:

  Should the stats be calculated on data before ("pre") or after
  ("post") filtering in specified step.

------------------------------------------------------------------------

### Method `show_help()`

Show source data or filter description

#### Usage

    Cohort$show_help(
      field,
      step_id,
      filter_id,
      modifier = getOption("cb_help_modifier", default = function(x) x)
    )

#### Arguments

- `field`:

  Name of the source description field provided as \`description\`
  argument to
  [set_source](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md).
  If missing, \`step_id\` and \`filter_id\` are used to return filter
  description.

- `step_id`:

  Id of the filter step to return description of.

- `filter_id`:

  Id of the filter to return description of.

- `modifier`:

  A function taking the description as argument. The function can be
  used to modify its argument (convert to html, display in browser
  etc.).

------------------------------------------------------------------------

### Method `get_code()`

Return reproducible data filtering code.

#### Usage

    Cohort$get_code(
      include_source = TRUE,
      include_methods = c(".pre_filtering", ".post_filtering", ".run_binding"),
      include_action = c("pre_filtering", "post_filtering", "run_binding"),
      modifier = .repro_code_tweak,
      mark_step = TRUE,
      ...
    )

#### Arguments

- `include_source`:

  If \`TRUE\` source generating code will be included.

- `include_methods`:

  Which methods definition should be included in the result.

- `include_action`:

  Which action should be returned in the result.
  \`pre_filtering\`/\`.post_filtering\` - to include data transformation
  before/after filtering. s\`run_binding\` - data binding
  transformation.

- `modifier`:

  A function taking data frame (storing reproducible code metadata) as
  an argument, and returning data frame with \`expr\` column which is
  then combined into a single expression (final result of \`get_code\`).
  See
  [.repro_code_tweak](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md).

- `mark_step`:

  Include information which filtering step is performed.

- `...`:

  Other parameters passed to
  [tidy_source](https://rdrr.io/pkg/formatR/man/tidy_source.html).

------------------------------------------------------------------------

### Method `run_flow()`

Trigger data calculations sequentially.

#### Usage

    Cohort$run_flow(
      min_step,
      hook = list(pre = get_hook("pre_run_flow_hook"), post = get_hook("post_run_flow_hook"))
    )

#### Arguments

- `min_step`:

  Step id starting from the calculation will be started.

- `hook`:

  List of hooks describing methods before/after the Cohort is created.
  See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

------------------------------------------------------------------------

### Method `run_step()`

Trigger data calculations for selected step.

#### Usage

    Cohort$run_step(
      step_id,
      hook = list(pre = get_hook("pre_run_step_hook"), post = get_hook("post_run_step_hook"))
    )

#### Arguments

- `step_id`:

  Id of the step for which to run data calculation.

- `hook`:

  List of hooks describing methods before/after the Cohort is created.
  See
  [hooks](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  for more details.

------------------------------------------------------------------------

### Method `bind_data()`

Run data binding for selected step. See more at
[binding-keys](https://r-world-devs.github.io/cohortBuilder/reference/binding-keys.md).

#### Usage

    Cohort$bind_data(step_id)

#### Arguments

- `step_id`:

  Id of the step for which to bind the data.

------------------------------------------------------------------------

### Method `describe_state()`

Print defined steps configuration.

#### Usage

    Cohort$describe_state()

------------------------------------------------------------------------

### Method `get_step()`

Get selected step configuration.

#### Usage

    Cohort$get_step(step_id)

#### Arguments

- `step_id`:

  Id of the step to be returned.

------------------------------------------------------------------------

### Method `get_filter()`

Get selected filter configuration.

#### Usage

    Cohort$get_filter(step_id, filter_id, method = function(x) x)

#### Arguments

- `step_id`:

  Id of the step where filter is defined.

- `filter_id`:

  If of the filter to be returned.

- `method`:

  Custom function taking filters list as argument.

------------------------------------------------------------------------

### Method `update_cache()`

Update filter or step cache. Caching is saving step and filter attached
data statistics such as number of data rows, filter choices or
frequencies.

#### Usage

    Cohort$update_cache(step_id, filter_id, state = "post")

#### Arguments

- `step_id`:

  Id of the step for which caching should be applied. If \`filter_id\`
  is not missing, the parameter describes id of the step where filter
  should be found.

- `filter_id`:

  Id of the filter for which caching should be applied.

- `state`:

  Should caching be done on data before ("pre") or after ("post")
  filtering in specified step.

------------------------------------------------------------------------

### Method `get_cache()`

Return step of filter specific cache.

#### Usage

    Cohort$get_cache(step_id, filter_id, state = "post")

#### Arguments

- `step_id`:

  Id of the step for which cached data should be returned If
  \`filter_id\` is not missing, the parameter describes id of the step
  where filter should be found.

- `filter_id`:

  Id of the filter for which cache data should be returned.

- `state`:

  Should cache be returned on data before ("pre") or after ("post")
  filtering in specified step.

------------------------------------------------------------------------

### Method `list_active_filters()`

List active filters included in selected step.

#### Usage

    Cohort$list_active_filters(step_id)

#### Arguments

- `step_id`:

  Id of the step where filters should be found.

------------------------------------------------------------------------

### Method `last_step_id()`

Return id of the last existing step in Cohort.

#### Usage

    Cohort$last_step_id()

------------------------------------------------------------------------

### Method `modify()`

Helper method enabling to run non-standard operation on Cohort object.

#### Usage

    Cohort$modify(modifier)

#### Arguments

- `modifier`:

  Function of two arguments \`self\` and \`private\`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Cohort$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
