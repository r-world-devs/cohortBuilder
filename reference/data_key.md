# Define Source dataset key

Data keys are used to define
[primary_keys](https://r-world-devs.github.io/cohortBuilder/reference/primary_keys.md)
and
[binding-keys](https://r-world-devs.github.io/cohortBuilder/reference/binding-keys.md).

## Usage

``` r
data_key(dataset, key)
```

## Arguments

- dataset:

  Name of the dataset included in Source.

- key:

  Character or character vector storing column names to be used as table
  keys.

## Value

\`data_key\` class list of two objects: \`dataset\` and \`key\` storing
name and vector of data key names respectively.
