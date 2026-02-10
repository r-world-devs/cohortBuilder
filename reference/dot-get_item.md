# Return list of objects matching provided condition.

Return list of objects matching provided condition.

## Usage

``` r
.get_item(list_obj, attribute, value, operator = `==`)
```

## Arguments

- list_obj:

  List of R objects.

- attribute:

  Object attribute name.

- value:

  Object value.

- operator:

  Logical operator - two-argument function taking \`list_obj\` attribute
  value as the first one, and \`value\` as the second one.

## Value

A subset of list object matching provided condition.

## Examples

``` r
my_list <- list(
  list(id = 1, name = "a"),
  list(id = 2, name = "b")
)
.get_item(my_list, "id", 1)
#> [[1]]
#> [[1]]$id
#> [1] 1
#> 
#> [[1]]$name
#> [1] "a"
#> 
#> 
.get_item(my_list, "name", c("b", "c"), identical)
#> list()
```
