# Define Source datasets primary keys

Primary keys can be defined as \`primary_keys\` parameter of
[set_source](https://r-world-devs.github.io/cohortBuilder/reference/set_source.md)
method. Currently, primary keys are used only to show keys information
in attrition plot (See
[attrition](https://r-world-devs.github.io/cohortBuilder/reference/attrition.md)).

## Usage

``` r
primary_keys(...)
```

## Arguments

- ...:

  Data keys describing tables primary keys.

## Value

List of class \`primary_keys\` storing
[data_key](https://r-world-devs.github.io/cohortBuilder/reference/data_key.md)s
objects.

## Examples

``` r
primary_keys(
  data_key('books', 'book_id'),
  data_key('borrowed', c('user_id', 'books_id', 'date'))
)
#> [[1]]
#> $dataset
#> [1] "books"
#> 
#> $key
#> [1] "book_id"
#> 
#> attr(,"class")
#> [1] "data_key"
#> 
#> [[2]]
#> $dataset
#> [1] "borrowed"
#> 
#> $key
#> [1] "user_id"  "books_id" "date"    
#> 
#> attr(,"class")
#> [1] "data_key"
#> 
#> attr(,"class")
#> [1] "primary_keys"
```
