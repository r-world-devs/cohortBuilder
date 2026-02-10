# Get function definition

Whenever the function with provided name exists anywhere, the one is
returned (or the first one if multiple found). Return NULL otherwise.

## Usage

``` r
.get_method(name)
```

## Arguments

- name:

  Name of the function.

## Value

Function - when found in any namespace or NULL otherwise.
