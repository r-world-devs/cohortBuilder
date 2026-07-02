# Source intelligence and AI tools

Beyond manually configuring filters, `cohortBuilder` can inspect a
source and describe or build filters for you. These features are also
the foundation for integrating a cohort with a Large Language Model
(LLM), so an assistant can explore the data and apply filters on the
user’s behalf.

This article covers four building blocks:

- [`describe()`](https://r-world-devs.github.io/cohortBuilder/reference/describe.md) -
  attach human-readable descriptions to datasets and variables,
- [`autofilter()`](https://r-world-devs.github.io/cohortBuilder/reference/autofilter.md) -
  auto-generate filters from the data structure,
- [`shape()`](https://r-world-devs.github.io/cohortBuilder/reference/shape.md) -
  return a structured summary of datasets and filters,
- AI tools (`R/ai_tools.R`) - expose the cohort to an `ellmer` chat.

## Describing a source

[`describe()`](https://r-world-devs.github.io/cohortBuilder/reference/describe.md)
builds a small description object (its text plus any extra fields) that
you attach to a source via the `description` argument of
[`set_source()`](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md).

The `description` is a nested list keyed by dataset name. Within each
dataset, the special key `dataset_` describes the dataset itself, and
any other key describes a variable of that dataset:

``` r

iris_source <- set_source(
  tblist(iris = iris),
  description = list(
    iris = list(
      dataset_ = describe("Edgar Anderson's measurements of iris flowers."),
      Species = describe("Iris species.", domain = c("setosa", "versicolor", "virginica"))
    )
  )
)
```

Extra named arguments to
[`describe()`](https://r-world-devs.github.io/cohortBuilder/reference/describe.md)
(such as `domain` above) are stored alongside the text and can be picked
up by other features - for example
[`autofilter()`](https://r-world-devs.github.io/cohortBuilder/reference/autofilter.md)
uses a supplied `domain` instead of scanning the data.

[`describe()`](https://r-world-devs.github.io/cohortBuilder/reference/describe.md)
also accepts a `label` - a short, human-readable name for the field.
When the field describes a variable,
[`autofilter()`](https://r-world-devs.github.io/cohortBuilder/reference/autofilter.md)
reuses the label as the generated filter’s `name` (the underlying
`variable` is unchanged), which is handy for giving filters friendlier
names in a GUI:

``` r

labelled_source <- set_source(
  tblist(iris = iris),
  description = list(
    iris = list(
      Species = describe("the species of iris", label = "Iris species")
    )
  )
) |>
  autofilter(attach_as = "meta")

species_filter <- purrr::detect(
  labelled_source$available_filters, ~ .x@id == "iris-Species"
)
species_filter@name
#> [1] "Iris species"
```

## Generating filters automatically

[`autofilter()`](https://r-world-devs.github.io/cohortBuilder/reference/autofilter.md)
analyses each column of the source and creates a filter suited to its
type (using filter rules such as `rule_character`, `rule_factor`,
`rule_numeric`, `rule_Date`, `rule_POSIXct`). The mapping is roughly:

| Column type | Filter type |
|----|----|
| character / factor | `discrete` (or `discrete_text` when all values are unique) |
| numeric / integer | `range` |
| Date | `date_range` |
| POSIXct | `datetime_range` |

The `attach_as` argument controls where the generated filters go.

With `attach_as = "step"` (the default) the filters are added as a
filtering step, so the cohort is immediately filterable:

``` r

iris_cohort <- set_source(tblist(iris = iris)) |>
  autofilter(attach_as = "step") |>
  cohort()

sum_up(iris_cohort)
#> >> Step ID: 1 [pending]
#> -> Filter ID: iris-SepalLength
#>    Filter Type: range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 4.3, 7.9
#>      dataset: iris
#>      variable: Sepal.Length
#>      range: NA
#>      keep_na: TRUE
#> -> Filter ID: iris-SepalWidth
#>    Filter Type: range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 2, 4.4
#>      dataset: iris
#>      variable: Sepal.Width
#>      range: NA
#>      keep_na: TRUE
#> -> Filter ID: iris-PetalLength
#>    Filter Type: range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 1, 6.9
#>      dataset: iris
#>      variable: Petal.Length
#>      range: NA
#>      keep_na: TRUE
#> -> Filter ID: iris-PetalWidth
#>    Filter Type: range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 0.1, 2.5
#>      dataset: iris
#>      variable: Petal.Width
#>      range: NA
#>      keep_na: TRUE
#> -> Filter ID: iris-Species
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: setosa, versicolor, virginica
#>      dataset: iris
#>      variable: Species
#>      value: NA
#>      keep_na: TRUE
```

With `attach_as = "meta"` the filters are stored in
`source$available_filters` rather than applied. This is the “menu” of
filters a GUI or an LLM can choose from, without forcing them onto the
data:

``` r

meta_source <- iris_source |>
  autofilter(attach_as = "meta")

length(meta_source$available_filters)
#> [1] 5
```

When a `domain` was provided via
[`describe()`](https://r-world-devs.github.io/cohortBuilder/reference/describe.md),
the generated filter inherits it instead of scanning the data:

``` r

species_filter <- purrr::detect(
  meta_source$available_filters, ~ .x@id == "iris-Species"
)
species_filter@domain
#> [1] "setosa"     "versicolor" "virginica"
```

## Inspecting a source with `shape()`

`shape(source)` returns a structured `list(datasets, filters)`
describing the source - ideal for programmatic inspection or passing to
an LLM:

- `datasets` maps each dataset name to its description text (or `NA`).
- `filters` is keyed by filter id; each entry is a list with `name`,
  `dataset`, `type`, `description`, `variables`, and `domain`. The
  `description` combines the filter-level description with the
  per-variable descriptions (single- variable filters show the bare
  variable description; multi-variable filters prefix each with its
  variable name).

``` r

result <- shape(meta_source)

# Dataset descriptions
result$datasets
#> $iris
#> [1] "Edgar Anderson's measurements of iris flowers."

# One filter entry
str(result$filters$`iris-Species`)
#> List of 6
#>  $ name       : chr "Species"
#>  $ dataset    : chr "iris"
#>  $ type       : chr "discrete"
#>  $ description: chr "Iris species."
#>  $ variables  :List of 1
#>   ..$ :List of 2
#>   .. ..$ name       : chr "Species"
#>   .. ..$ description: chr "Iris species."
#>  $ domain     : chr [1:3] "setosa" "versicolor" "virginica"
```

When a filter’s own `@domain` is unset,
[`shape()`](https://r-world-devs.github.io/cohortBuilder/reference/shape.md)
falls back to the domain stored in the source’s metadata statistics, so
the `domain` field is populated whenever possible.

**Note.** Called with a `field` (and optional `subfield`),
`shape(source, field, subfield)` instead performs a description-text
lookup - this is the form used internally by `Cohort$show_help()`.

## Connecting a cohort to an LLM

The functions in `R/ai_tools.R` wrap cohort operations as tools an
[`ellmer`](https://ellmer.tidyverse.org/) chat can call. Each tool is a
`cb_tool` object (a function plus a name, description, and argument
schema).

The built-in tool factories each take a cohort and return a `cb_tool`:

| Tool factory | Purpose |
|----|----|
| [`cb_tool_filters_meta()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_filters_meta.md) | Return available-filter metadata (via [`shape()`](https://r-world-devs.github.io/cohortBuilder/reference/shape.md)) as JSON |
| [`cb_tool_describe_state()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_describe_state.md) | Describe current steps, filters, and pending state |
| [`cb_tool_get_data_summary()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_get_data_summary.md) | Report row counts per dataset and step |
| [`cb_tool_get_code()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_get_code.md) | Return reproducible filtering code |
| [`cb_tool_add_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_add_filters.md) | Add filters (no values) to a new or existing step |
| [`cb_tool_set_filter_values()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_set_filter_values.md) | Set values on existing filters |
| [`cb_tool_apply_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_apply_filters.md) | Add filters and set their values in one call |
| [`cb_tool_toggle_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_toggle_filters.md) | Activate / deactivate filters |
| [`cb_tool_clear_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_clear_filters.md) | Reset filters to their defaults |
| [`cb_tool_remove_filters()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_remove_filters.md) | Remove filters from a step |
| [`cb_tool_remove_step()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_remove_step.md) | Remove the last step |
| [`cb_tool_run()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool_run.md) | Run the pipeline (when auto-run is disabled) |

A `cb_tool` prints its name, description, and arguments:

``` r

coh <- cohort(meta_source)
tool <- cb_tool_filters_meta(coh)
print(tool)
#> cohortBuilder tool: cb_get_filters_meta 
#> Description: Returns metadata about the datasets and available filters in JSON format. The JSON has two top-level keys: 'datasets' - an object keyed by dataset name, mapping each dataset to its description (or null). 'filters' - an object keyed by filter id, where each value describes one filter with fields: 'name' (the filter's display name), 'dataset' (the dataset the filter belongs to), 'type' (the filter type, e.g. 'discrete', 'range', 'date_range'), 'description' (a human-readable summary combining the filter's purpose and its variables, or null), 'variables' (an array of objects, each with 'name' and 'description', for the columns the filter covers), 'domain' (the set of valid values the filter accepts: an array of allowed values for discrete-type filters, or a two-element [min, max] array for range-type filters). Use the filter id keys when referring to filters in other tools.
```

For LLM-driven filtering to work, the source must expose a menu of
filters via `autofilter(attach_as = "meta")` so the assistant knows what
it can apply.

To register tools with an `ellmer` chat, use
[`cb_register_tool()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_register_tool.md)
for a single tool or
[`cb_register_tools()`](https://r-world-devs.github.io/cohortBuilder/reference/cb_register_tool.md)
to register all of them at once:

``` r

library(ellmer)

source <- set_source(tblist(iris = iris)) |>
  autofilter(attach_as = "meta")
coh <- cohort(source)

chat <- chat_openai()
chat |> cb_register_tools(coh)

chat$chat("Filter the data to setosa flowers with sepal length over 5")
```

By default the cohort runs automatically after each tool modifies it.
Set `options(cb_tool_run_cohort = FALSE)` to require an explicit
`cb_run` call instead.

To trace which tools the LLM invokes (and with which arguments), set
`options(cb_tool_verbose = TRUE)`. Each call then emits an informative
[`message()`](https://rdrr.io/r/base/message.html) such as
`[cohortBuilder AI tool] cb_apply_filters (filters = ...; action = new_step)`.
Logging is off by default, so tools stay silent during normal use.

**Note.** The AI tools require the suggested `ellmer` package.
