# Register a custom filter type

Registers an S7 filter constructor so it can be used with
[`filter`](https://r-world-devs.github.io/cohortBuilder/reference/filter.md)`("type", ...)`.
The constructor must return an object inheriting from
[CbFilter](https://r-world-devs.github.io/cohortBuilder/reference/CbFilter.md).

## Usage

``` r
register_filter_type(type, constructor)
```

## Arguments

- type:

  Character string identifying the filter type.

- constructor:

  S7 class constructor (e.g. created with \[S7::new_class()\]).

## Examples

``` r
if (FALSE) { # \dontrun{
MyCbFilter <- S7::new_class("MyCbFilter",
  parent = CbFilter,
  package = "mypackage",
  properties = list(dataset = S7::class_character, variable = S7::class_character),
  constructor = function(id = NULL, name = NULL, variable, dataset,
                         description = NULL, domain = NULL, ...) {
    id <- id %||% .default_filter_id(dataset, variable)
    name <- name %||% id
    S7::new_object(S7::S7_object(),
      type = "my_filter", id = id, name = name,
      dataset = dataset, variable = variable,
      active = TRUE, description = description, domain = domain,
      extra = list(...), private = list(input_param = "value")
    )
  }
)
register_filter_type("my_filter", MyCbFilter)
# Now filter("my_filter", ...) works
} # }
```
