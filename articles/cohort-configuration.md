# Variants for Cohort Configuration

When working with `cohortBuilder` you can configure filtering steps
multiple ways. All the possible ways are defined in this article.

## Filtering steps in Source

When filtering steps are configured inside Source object, `cohort`
automatically inherits them.

You can achieve configuring filtering steps in Source using `add_step`
method:

``` r
librarian_source <- set_source(
  as.tblist(librarian)
) %>% 
  add_step(
    step(
      filter(
        "discrete", id = "author", dataset = "books", 
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete", id = "program", dataset = "borrowers", 
        variable = "program", value = "premium", keep_na = FALSE
      )
    )
  )
```

or with `%->%` pipe operator:

``` r
librarian_source <- set_source(
  as.tblist(librarian)
) %->% 
  step(
    filter(
      "discrete", id = "author", dataset = "books", 
      variable = "author", value = "Dan Brown"
    ),
    filter(
      "discrete", id = "program", dataset = "borrowers", 
      variable = "program", value = "premium", keep_na = FALSE
    )
  )
```

You can also configure filtering steps using `add_filter` methods,
passing `step_id` inside:

``` r
librarian_source <- set_source(
  as.tblist(librarian)
) %>% 
  add_filter(
    filter(
      "discrete", id = "author", dataset = "books", 
      variable = "author", value = "Dan Brown"
    ),
    step_id = 1
  ) %>% 
  add_filter(
    filter(
      "discrete", id = "program", dataset = "borrowers", 
      variable = "program", value = "premium", keep_na = FALSE
    ),
    step_id = 1
  )
```

**Note.** When `step_id` is skipped, the filter is added to the last
existing step (or the first one if no steps exist).

Or even simpler using `%->%` (to put filters in the last existing step):

``` r
librarian_source <- set_source(
  as.tblist(librarian)
) %->% 
  filter(
    "discrete", id = "author", dataset = "books", 
    variable = "author", value = "Dan Brown"
  ) %->% 
  filter(
    "discrete", id = "program", dataset = "borrowers", 
    variable = "program", value = "premium", keep_na = FALSE
  )
```

Then, create cohort with:

``` r
librarian_cohort <- cohort(librarian_source)
sum_up(librarian_cohort)
#> >> Step ID: 1
#> -> Filter ID: author
#>    Filter Type: discrete
#>    Filter Parameters:
#>      dataset: books
#>      variable: author
#>      value: Dan Brown
#>      keep_na: TRUE
#>      description: 
#>      active: TRUE
#> -> Filter ID: program
#>    Filter Type: discrete
#>    Filter Parameters:
#>      dataset: borrowers
#>      variable: program
#>      value: premium
#>      keep_na: FALSE
#>      description: 
#>      active: TRUE
```

## Filtering steps in Cohort

When filtering steps are not configured in the Source, you can always
achieve it using `Cohort` methods.

The standard way is to place steps configuration while creating Cohort:

``` r
librarian_source <- set_source(
  as.tblist(librarian)
)

librarian_cohort <- librarian_source %>% 
  cohort(
    step(
      filter(
        "discrete", id = "author", dataset = "books", 
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete", id = "program", dataset = "borrowers", 
        variable = "program", value = "premium", keep_na = FALSE
      )
    )
  )
```

Or if you want to define only one step, place filters directly:

``` r
librarian_cohort <- librarian_source %>% 
  cohort(
    filter(
      "discrete", id = "author", dataset = "books", 
      variable = "author", value = "Dan Brown"
    ),
    filter(
      "discrete", id = "program", dataset = "borrowers", 
      variable = "program", value = "premium", keep_na = FALSE
    )
  )
```

In case when Cohort is already defined, you can repeat any approach we
presented while adding filtering steps to source.

Using `add_step`:

``` r
librarian_cohort <- librarian_source %>% cohort()

librarian_cohort %>% 
  add_step(
    step(
      filter(
        "discrete", id = "author", dataset = "books", 
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete", id = "program", dataset = "borrowers", 
        variable = "program", value = "premium", keep_na = FALSE
      )
    )
  )
  
```

Using `%->%` pipe operator:

``` r
librarian_cohort <- librarian_source %>% cohort()

librarian_cohort %->% 
  step(
    filter(
      "discrete", id = "author", dataset = "books", 
      variable = "author", value = "Dan Brown"
    ),
    filter(
      "discrete", id = "program", dataset = "borrowers", 
      variable = "program", value = "premium", keep_na = FALSE
    )
  )
```

You can also configure filtering steps using `add_filter` methods,
passing `step_id` inside:

``` r
librarian_cohort <- librarian_source %>% cohort()

librarian_cohort %>% 
  add_filter(
    filter(
      "discrete", id = "author", dataset = "books", 
      variable = "author", value = "Dan Brown"
    )
  ) %>% 
  add_filter(
    filter(
      "discrete", id = "program", dataset = "borrowers", 
      variable = "program", value = "premium", keep_na = FALSE
    )
  )
```

**Note.** When `step_id` is skipped, the filter is added to the last
existing step (or the first one if no steps exist).

Or even simpler using `%->%` (to put filters in the last existing step):

``` r
librarian_cohort <- librarian_source %>% cohort()

librarian_cohort %->% 
  filter(
    "discrete", id = "author", dataset = "books", 
    variable = "author", value = "Dan Brown"
  ) %->% 
  filter(
    "discrete", id = "program", dataset = "borrowers", 
    variable = "program", value = "premium", keep_na = FALSE
  )
```

As usual we can verify the configuration with `sum_up`:

``` r
sum_up(librarian_cohort)
#> >> Step ID: 1
#> -> Filter ID: author
#>    Filter Type: discrete
#>    Filter Parameters:
#>      dataset: books
#>      variable: author
#>      value: Dan Brown
#>      keep_na: TRUE
#>      description: 
#>      active: TRUE
#> -> Filter ID: program
#>    Filter Type: discrete
#>    Filter Parameters:
#>      dataset: borrowers
#>      variable: program
#>      value: premium
#>      keep_na: FALSE
#>      description: 
#>      active: TRUE
```
