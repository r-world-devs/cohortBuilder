# Package index

## All functions

- [`CbFilter()`](https://r-world-devs.github.io/cohortBuilder/reference/CbFilter.md)
  : Base class for all cohortBuilder filters
- [`CbFilterDateRange()`](https://r-world-devs.github.io/cohortBuilder/reference/CbFilterDateRange.md)
  : Date range filter class
- [`CbFilterDatetimeRange()`](https://r-world-devs.github.io/cohortBuilder/reference/CbFilterDatetimeRange.md)
  : Datetime range filter class
- [`CbFilterDiscrete()`](https://r-world-devs.github.io/cohortBuilder/reference/CbFilterDiscrete.md)
  : Discrete filter class
- [`CbFilterDiscreteText()`](https://r-world-devs.github.io/cohortBuilder/reference/CbFilterDiscreteText.md)
  : Discrete text filter class
- [`CbFilterMultiDiscrete()`](https://r-world-devs.github.io/cohortBuilder/reference/CbFilterMultiDiscrete.md)
  : Multi-discrete filter class
- [`CbFilterQuery()`](https://r-world-devs.github.io/cohortBuilder/reference/CbFilterQuery.md)
  : Query filter class
- [`CbFilterRange()`](https://r-world-devs.github.io/cohortBuilder/reference/CbFilterRange.md)
  : Range filter class
- [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  : R6 class representing Cohort object.
- [`Source`](https://r-world-devs.github.io/cohortBuilder/reference/Source.md)
  : R6 class representing a data source
- [`add_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/add_filter.md)
  : Add filter definition
- [`add_source()`](https://r-world-devs.github.io/cohortBuilder/reference/add_source.md)
  : Add source to Cohort object.
- [`add_step()`](https://r-world-devs.github.io/cohortBuilder/reference/add_step.md)
  : Add filtering step definition
- [`attrition()`](https://r-world-devs.github.io/cohortBuilder/reference/attrition.md)
  : Show attrition plot.
- [`autofilter()`](https://r-world-devs.github.io/cohortBuilder/reference/autofilter.md)
  : Generate filters definition based on the Source data
- [`bind_keys()`](https://r-world-devs.github.io/cohortBuilder/reference/binding-keys.md)
  [`bind_key()`](https://r-world-devs.github.io/cohortBuilder/reference/binding-keys.md)
  : Describe data relations with binding keys
- [`cb_domain_from_data()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_domain_from_data.md)
  : Extract domain from data
- [`cb_domain_from_stats()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_domain_from_stats.md)
  : Extract domain from stored filter statistics
- [`cb_filter_data()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_filter_data.md)
  : Apply filter to data object
- [`cb_filter_to_expr()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_filter_to_expr.md)
  : Generate reproducible code expression for filter
- [`cb_get_filter_data()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_get_filter_data.md)
  : Get filter-related data
- [`cb_get_filter_defaults()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_get_filter_defaults.md)
  : Get filter default values
- [`cb_get_filter_stats()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_get_filter_stats.md)
  : Get filter statistics
- [`cb_intersect_domain()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_intersect_domain.md)
  : Get effective filter value after domain intersection
- [`cb_intersect_domain_values()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_intersect_domain_values.md)
  : Intersect two domain values for a filter type
- [`cb_plot_filter_data()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_plot_filter_data.md)
  : Plot filter data
- [`cb_register_tool()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_register_tool.md)
  [`cb_register_tools()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_register_tool.md)
  : Register cohortBuilder tools with an ellmer chat
- [`cb_tool()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
  [`print(`*`<cb_tool>`*`)`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
  : Create a cohortBuilder tool definition
- [`cb_tool_add_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_add_filters.md)
  : Create a tool for adding filters to a cohort
- [`cb_tool_apply_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_apply_filters.md)
  : Create a tool that adds filters and sets their values in one call
- [`cb_tool_clear_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_clear_filters.md)
  : Create a tool for resetting filters to defaults
- [`cb_tool_describe_state()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_describe_state.md)
  : Create a tool returning the current cohort state
- [`cb_tool_filters_meta()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_filters_meta.md)
  : Create a tool returning available filters metadata
- [`cb_tool_get_code()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_get_code.md)
  : Create a tool returning reproducible filtering code
- [`cb_tool_get_data_summary()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_get_data_summary.md)
  : Create a tool returning row counts per dataset and step
- [`cb_tool_remove_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_remove_filters.md)
  : Create a tool for removing filters from the cohort
- [`cb_tool_remove_step()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_remove_step.md)
  : Create a tool for removing the last step
- [`cb_tool_run()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_run.md)
  : Create a tool for running the cohort pipeline
- [`cb_tool_set_filter_values()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_set_filter_values.md)
  : Create a tool for setting filter values
- [`cb_tool_toggle_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_toggle_filters.md)
  : Create a tool for activating or deactivating filters
- [`code()`](https://r-world-devs.github.io/cohortBuilder/reference/code.md)
  : Return reproducible data filtering code.
- [`cohort-methods`](https://r-world-devs.github.io/cohortBuilder/reference/cohort-methods.md)
  : Cohort related methods
- [`cohortBuilder-package`](https://r-world-devs.github.io/cohortBuilder/reference/cohortBuilder-package.md)
  : Create data source cohort
- [`cohort()`](https://r-world-devs.github.io/cohortBuilder/reference/create-cohort.md)
  : Create new 'Cohort' object
- [`data_key()`](https://r-world-devs.github.io/cohortBuilder/reference/data_key.md)
  : Define Source dataset key
- [`describe()`](https://r-world-devs.github.io/cohortBuilder/reference/describe.md)
  : Create a description object
- [`description()`](https://r-world-devs.github.io/cohortBuilder/reference/description.md)
  : Show source data or filter description
- [`.gen_id()`](https://r-world-devs.github.io/cohortBuilder/reference/dot-gen_id.md)
  : Generate random ID
- [`.get_item()`](https://r-world-devs.github.io/cohortBuilder/reference/dot-get_item.md)
  : Return list of objects matching provided condition.
- [`.get_method()`](https://r-world-devs.github.io/cohortBuilder/reference/dot-get_method.md)
  : Get function definition
- [`.if_value()`](https://r-world-devs.github.io/cohortBuilder/reference/dot-if_value.md)
  : Return default value if values are equal
- [`.print_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/dot-print_filter.md)
  : Method for printing filter details
- [`.propagate_domains()`](https://r-world-devs.github.io/cohortBuilder/reference/dot-propagate_domains.md)
  : Propagate domains between steps
- [`filter()`](https://r-world-devs.github.io/cohortBuilder/reference/filter.md)
  : Define Cohort filter
- [`filter_domain()`](https://r-world-devs.github.io/cohortBuilder/reference/filter_domain.md)
  : Get a filter's domain
- [`filter_effective_value()`](https://r-world-devs.github.io/cohortBuilder/reference/filter_effective_value.md)
  : Get a filter's effective value
- [`filter_variables()`](https://r-world-devs.github.io/cohortBuilder/reference/filter_variables.md)
  : Get the variables a filter operates on
- [`get_data()`](https://r-world-devs.github.io/cohortBuilder/reference/get_data.md)
  : Get step related data
- [`get_filter_params()`](https://r-world-devs.github.io/cohortBuilder/reference/get_filter_params.md)
  : Get filter parameters as a list
- [`get_state()`](https://r-world-devs.github.io/cohortBuilder/reference/get_state.md)
  : Get Cohort configuration state.
- [`` `%->%` ``](https://r-world-devs.github.io/cohortBuilder/reference/grapes-greater-than-grapes.md)
  : Operator simplifying adding steps or filters to Cohort and Source
  objects
- [`add_hook()`](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  [`get_hook()`](https://r-world-devs.github.io/cohortBuilder/reference/hooks.md)
  : Cohort hooks.
- [`librarian`](https://r-world-devs.github.io/cohortBuilder/reference/librarian.md)
  : Sample of library database
- [`managing-cohort`](https://r-world-devs.github.io/cohortBuilder/reference/managing-cohort.md)
  : Managing the Cohort object
- [`managing-source`](https://r-world-devs.github.io/cohortBuilder/reference/managing-source.md)
  : Managing the Source object
- [`plot_data()`](https://r-world-devs.github.io/cohortBuilder/reference/plot_data.md)
  : Plot filter related Cohort data.
- [`primary_keys()`](https://r-world-devs.github.io/cohortBuilder/reference/primary_keys.md)
  : Define Source datasets primary keys
- [`register_filter_type()`](https://r-world-devs.github.io/cohortBuilder/reference/register_filter_type.md)
  : Register a custom filter type
- [`restore()`](https://r-world-devs.github.io/cohortBuilder/reference/restore.md)
  : Restore Cohort object.
- [`rm_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/rm_filter.md)
  : Remove filter definition
- [`rm_step()`](https://r-world-devs.github.io/cohortBuilder/reference/rm_step.md)
  : Remove filtering step definition
- [`run()`](https://r-world-devs.github.io/cohortBuilder/reference/run.md)
  : Trigger data calculations.
- [`set_source()`](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md)
  : Create Cohort source
- [`shape()`](https://r-world-devs.github.io/cohortBuilder/reference/shape.md)
  : Describe the structure of a source
- [`.init_step()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  [`.collect_data()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  [`.get_stats()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  [`.pre_filtering()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  [`.post_filtering()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  [`.post_binding()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  [`.repro_code_tweak()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  [`.get_attrition_label()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  [`.get_attrition_count()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  [`.run_binding()`](https://r-world-devs.github.io/cohortBuilder/reference/source-layer.md)
  : Source compatibility methods.
- [`stat()`](https://r-world-devs.github.io/cohortBuilder/reference/stat.md)
  : Get Cohort related statistics.
- [`step()`](https://r-world-devs.github.io/cohortBuilder/reference/step.md)
  : Create filtering step
- [`sum_up()`](https://r-world-devs.github.io/cohortBuilder/reference/sum_up.md)
  : Sum up Cohort state.
- [`tblist()`](https://r-world-devs.github.io/cohortBuilder/reference/tblist.md)
  [`as.tblist()`](https://r-world-devs.github.io/cohortBuilder/reference/tblist.md)
  : Create in memory tables connection
- [`tblist_class`](https://r-world-devs.github.io/cohortBuilder/reference/tblist_class.md)
  : S7 class wrapper for the \`tblist\` source
- [`update_filter()`](https://r-world-devs.github.io/cohortBuilder/reference/update_filter.md)
  : Update filter definition
- [`update_source()`](https://r-world-devs.github.io/cohortBuilder/reference/update_source.md)
  : Update source in Cohort object.
