# Create a cohortBuilder tool definition

Constructs a tool object that can be registered with an LLM chat via
[`cb_register_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_register_tool.md).

## Usage

``` r
cb_tool(fun, name, description, arguments = list())

# S3 method for class 'cb_tool'
print(x, ...)
```

## Arguments

- fun:

  Function to invoke when the tool is called.

- name:

  Character string identifying the tool.

- description:

  Character string describing what the tool does. The more detail
  provided, the better the LLM can decide when to use it.

- arguments:

  Named list of argument type definitions created by ellmer `type_*()`
  functions (e.g.
  [`ellmer::type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html)).

- x:

  A `cb_tool` object.

- ...:

  Ignored.

## Value

An object of class `cb_tool`.
