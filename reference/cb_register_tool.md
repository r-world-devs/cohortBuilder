# Register cohortBuilder tools with an ellmer chat

`cb_register_tool` registers a single
[`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
with an ellmer chat object. `cb_register_tools` is a convenience wrapper
that registers all built-in tools at once.

## Usage

``` r
cb_register_tool(chat, tool)

cb_register_tools(chat, cohort)
```

## Arguments

- chat:

  An ellmer chat object (e.g. from
  [`ellmer::chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.html)).

- tool:

  A
  [`cb_tool`](https://r-world-devs.github.io/cohortBuilder/reference/cb_tool.md)
  object.

- cohort:

  A
  [`Cohort`](https://r-world-devs.github.io/cohortBuilder/reference/Cohort.md)
  object.

## Value

The `chat` object, invisibly (for piping).

## Examples

``` r
if (FALSE) { # \dontrun{
source <- set_source(tblist(iris = iris)) |> autofilter(attach_as = "meta")
coh <- cohort(source)
chat <- ellmer::chat_openai()
chat |> cb_register_tools(coh)
chat$chat("Show me the available filters")
} # }
```
