# Generate filters definition based on the Source data

The method should analyze source data structure, generate proper filters
based on the data (e.g. column types) and attach them to source.

## Usage

``` r
autofilter(source, attach_as = c("step", "meta"), ...)

# Default S3 method
autofilter(source, ...)

# S3 method for class 'tblist'
autofilter(source, attach_as = c("step", "meta"), ...)
```

## Arguments

- source:

  Source object.

- attach_as:

  Choose whether the filters should be attached as a new step, or list
  of available filters (used in filtering panel when \`new_step =
  "configure"\`). By default in `step`.

- ...:

  Extra arguments passed to a specific method.

## Value

Source object having step configuration attached.

## Examples

``` r
library(cohortBuilder)

iris_source <- set_source(tblist(iris = iris)) |>
  autofilter()
iris_cohort <- cohort(iris_source)
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
