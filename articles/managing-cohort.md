# Managing Cohort Object

When working with already defined cohort, you may want to manipulate its
configuration (i.e. filter value) without the need to create the cohort
from scratch.

`cohortBuilder` offers various methods that perform common Cohort
management operations.

To present the functionality we’ll be working on the below
`librarian_cohort` object:

``` r

librarian_source <- set_source(
  as.tblist(librarian)
)

librarian_cohort <- librarian_source |>
  cohort(
    step(
      filter(
        "discrete",
        id = "author", dataset = "books",
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete",
        id = "program", dataset = "borrowers",
        variable = "program", value = "premium", keep_na = FALSE
      )
    ),
    step(
      filter(
        "range",
        id = "copies", dataset = "books",
        variable = "copies", range = c(-Inf, 5L)
      )
    ),
    run_flow = TRUE
  )
```

## Managing filters

In order to manage filters configuration you may call the following
methods:

- `update_filter` - to update filter configuration,
- `add_filter` - to add new filter in the selected step,
- `rm_filter` - to remove filter in the existing step.

Updating filter:

``` r

librarian_cohort |>
  update_filter(
    step_id = 1L, filter_id = "author", value = c("Dan Brown", "Khaled Hosseini")
  )

sum_up(librarian_cohort)
#> >> Step ID: 1 [pending]
#> -> Filter ID: author
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: books
#>      variable: author
#>      value: Dan Brown, Khaled Hosseini
#>      keep_na: TRUE
#> -> Filter ID: program
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: borrowers
#>      variable: program
#>      value: premium
#>      keep_na: FALSE
#> >> Step ID: 2 [pending]
#> -> Filter ID: copies
#>    Filter Type: range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: books
#>      variable: copies
#>      range: -Inf, 5
#>      keep_na: TRUE
```

Adding new filter:

``` r

librarian_cohort |>
  add_filter(
    filter(
      "date_range",
      id = "issue_date", dataset = "issues",
      variable = "date", range = c(as.Date("2010-01-01"), Inf)
    ),
    step_id = 2L
  )

sum_up(librarian_cohort)
#> >> Step ID: 1 [pending]
#> -> Filter ID: author
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: books
#>      variable: author
#>      value: Dan Brown, Khaled Hosseini
#>      keep_na: TRUE
#> -> Filter ID: program
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: borrowers
#>      variable: program
#>      value: premium
#>      keep_na: FALSE
#> >> Step ID: 2 [pending]
#> -> Filter ID: copies
#>    Filter Type: range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: books
#>      variable: copies
#>      range: -Inf, 5
#>      keep_na: TRUE
#> -> Filter ID: issue_date
#>    Filter Type: date_range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: issues
#>      variable: date
#>      range: 2010-01-01, Inf
#>      keep_na: TRUE
```

Removing filter:

``` r

librarian_cohort |>
  rm_filter(step_id = 2L, filter_id = "copies")

sum_up(librarian_cohort)
#> >> Step ID: 1 [pending]
#> -> Filter ID: author
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: books
#>      variable: author
#>      value: Dan Brown, Khaled Hosseini
#>      keep_na: TRUE
#> -> Filter ID: program
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: borrowers
#>      variable: program
#>      value: premium
#>      keep_na: FALSE
#> >> Step ID: 2 [pending]
#> -> Filter ID: issue_date
#>    Filter Type: date_range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: issues
#>      variable: date
#>      range: 2010-01-01, Inf
#>      keep_na: TRUE
```

By default the above configuration doesn’t trigger data recalculation so
we need to call `run` method.

Calling `run` we trigger all steps computations. In our case we’ve
updated only the second step so we can optimize workflow skipping the
previous steps calculation by specifying `min_step_id` parameter:

``` r

run(librarian_cohort, min_step_id = 2L)

get_data(librarian_cohort)
#> $books
#> # A tibble: 2 × 6
#>   isbn          title             genre                  publisher author copies
#>   <chr>         <chr>             <chr>                  <chr>     <chr>   <int>
#> 1 0-385-50420-9 The Da Vinci Code Crime, Thriller & Adv… Transwor… Dan B…      7
#> 2 0-671-02735-2 Angels and Demons Crime, Thriller & Adv… Transwor… Dan B…      4
#> 
#> $borrowers
#> # A tibble: 6 × 6
#>   id     registered address                           name  phone_number program
#>   <chr>  <date>     <chr>                             <chr> <chr>        <chr>  
#> 1 000001 2001-06-09 66 N. Evergreen Ave. Norristown,… Mrs.… 626-594-4729 premium
#> 2 000005 2005-01-15 580 Chapel Rd. Delray Beach, FL … Ferd… 127-363-0738 premium
#> 3 000008 2006-11-15 9533 Delaware Dr. Peabody, MA 01… Mrs.… 460-779-8714 premium
#> 4 000011 2009-03-24 745 E. Sussex Drive Mahwah, NJ 0… Mr. … 378-884-6509 premium
#> 5 000013 2011-09-30 534 Iroquois Ave. Watertown, MA … Dr. … 104-832-8013 premium
#> # ℹ 1 more row
#> 
#> $issues
#> # A tibble: 42 × 4
#>   id     borrower_id isbn          date      
#>   <chr>  <chr>       <chr>         <date>    
#> 1 000001 000019      0-676-97976-9 2015-03-17
#> 2 000003 000016      0-09-177373-3 2014-09-28
#> 3 000006 000018      0-14-303714-5 2016-07-21
#> 4 000008 000016      0-340-89696-5 2016-04-16
#> 5 000009 000017      0-09-177373-3 2016-11-12
#> # ℹ 37 more rows
#> 
#> $returns
#> # A tibble: 30 × 2
#>   id     date      
#>   <chr>  <date>    
#> 1 000001 2015-04-06
#> 2 000003 2014-10-23
#> 3 000004 2005-12-29
#> 4 000005 2006-03-26
#> 5 000006 2016-08-30
#> # ℹ 25 more rows
#> 
#> attr(,"class")
#> [1] "tblist"
#> attr(,"call")
#> as.tblist(librarian)
```

**Note.** If you want to run data computation directly after calling one
of the above methods just set `run_flow = TRUE` within the method.

## Managing steps

Similar to filter, you can operate on the Cohort to manage steps.
`cohortBuilder` offers `add_step` and `rm_step` methods to add new, or
remove existing step respectively.

``` r

librarian_cohort |>
  rm_step(step_id = 1L)

sum_up(librarian_cohort)
#> >> Step ID: 1 [pending]
#> -> Filter ID: issue_date
#>    Filter Type: date_range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: issues
#>      variable: date
#>      range: 2010-01-01, Inf
#>      keep_na: TRUE
```

**Note.** Removing not the last step results with renaming all step ids
(so that we always have steps numbering starting with 1).

``` r

librarian_cohort |>
  add_step(
    step(
      filter(
        "discrete",
        id = "author", dataset = "books",
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete",
        id = "program", dataset = "borrowers",
        variable = "program", value = "premium", keep_na = FALSE
      )
    )
  )

sum_up(librarian_cohort)
#> >> Step ID: 1 [pending]
#> -> Filter ID: issue_date
#>    Filter Type: date_range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: issues
#>      variable: date
#>      range: 2010-01-01, Inf
#>      keep_na: TRUE
#> >> Step ID: 2 [pending]
#> -> Filter ID: author
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: books
#>      variable: author
#>      value: Dan Brown
#>      keep_na: TRUE
#> -> Filter ID: program
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: borrowers
#>      variable: program
#>      value: premium
#>      keep_na: FALSE
```

**Note.** All the methods used for managing steps and filters can be
also called on Source object itself. See
[`vignette("cohort-configuration")`](https://r-world-devs.github.io/cohortBuilder/articles/cohort-configuration.md).

## Managing source

The last Cohort configuration component - source, can be also managed
within the Cohort itself. With `update_source` method you can change the
source defined in the existing Cohort.

Below we update cohort with Source having `source_code` parameter
defined. The argument is responsible to generate `source` object
definition printed in the reproducible code (you can use it when the
default method doesn’t print reasonable output).

``` r

code(librarian_cohort, include_methods = NULL)
#> source <- list(dtconn = as.tblist(librarian))
#> data_object <- source$dtconn
#> step_id <- "1"
#> pre_data_object <- data_object
#> data_object <- .pre_filtering(source, data_object, "1")
#> data_object[["issues"]] <- dplyr::filter(data_object[["issues"]], (date <= Inf &
#>     date >= 14610) | is.na(date))
#> data_object <- .post_filtering(source, data_object, "1")
#> for (binding_key in binding_keys) {
#>     data_object <- .run_binding(source, binding_key, pre_data_object, data_object)
#> }
#> step_id <- "2"
#> data_object <- .pre_filtering(source, data_object, "2")
#> data_object[["books"]] <- dplyr::filter(data_object[["books"]], author %in% c("Dan Brown",
#>     NA))
#> data_object[["borrowers"]] <- dplyr::filter(data_object[["borrowers"]], program %in%
#>     "premium")
#> data_object <- .post_filtering(source, data_object, "2")

new_source <- set_source(
  as.tblist(librarian),
  source_code = quote({
    source <- list()
    source$dtconn <- as.tblist(librarian)
  })
)

update_source(librarian_cohort, new_source)
sum_up(librarian_cohort)
#> >> Step ID: 1 [pending]
#> -> Filter ID: issue_date
#>    Filter Type: date_range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: issues
#>      variable: date
#>      range: 2010-01-01, Inf
#>      keep_na: TRUE
#> >> Step ID: 2 [pending]
#> -> Filter ID: author
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: books
#>      variable: author
#>      value: Dan Brown
#>      keep_na: TRUE
#> -> Filter ID: program
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: borrowers
#>      variable: program
#>      value: premium
#>      keep_na: FALSE
code(librarian_cohort, include_methods = NULL)
#> source <- list()
#> source$dtconn <- as.tblist(librarian)
#> data_object <- source$dtconn
#> step_id <- "1"
#> pre_data_object <- data_object
#> data_object <- .pre_filtering(source, data_object, "1")
#> data_object[["issues"]] <- dplyr::filter(data_object[["issues"]], (date <= Inf &
#>     date >= 14610) | is.na(date))
#> data_object <- .post_filtering(source, data_object, "1")
#> for (binding_key in binding_keys) {
#>     data_object <- .run_binding(source, binding_key, pre_data_object, data_object)
#> }
#> step_id <- "2"
#> data_object <- .pre_filtering(source, data_object, "2")
#> data_object[["books"]] <- dplyr::filter(data_object[["books"]], author %in% c("Dan Brown",
#>     NA))
#> data_object[["borrowers"]] <- dplyr::filter(data_object[["borrowers"]], program %in%
#>     "premium")
#> data_object <- .post_filtering(source, data_object, "2")
```

Note that updating source doesn’t remove Cohort configuration (steps and
filters). If you want to clear the configuration just set
`keep_steps = FALSE`:

``` r

update_source(librarian_cohort, new_source, keep_steps = FALSE)
sum_up(librarian_cohort)
#> No steps configuration found.
```

You can also use `update_source` to add Source to an empty Cohort:

``` r

new_source <- set_source(
  as.tblist(librarian)
)
empty_cohort <- cohort()
update_source(empty_cohort, new_source)
code(empty_cohort, include_methods = NULL)
#> source <- list(dtconn = as.tblist(librarian))
#> data_object <- source$dtconn
```

The `update_source` method can be also useful if you want to update
source along with steps and filters configuration.

In this case, the good practice is to keep the configuration directly in
Source:

``` r

source_one <- set_source(
  as.tblist(librarian)
) |>
  add_step(
    step(
      filter(
        "discrete",
        id = "author", dataset = "books",
        variable = "author", value = "Dan Brown"
      ),
      filter(
        "discrete",
        id = "program", dataset = "borrowers",
        variable = "program", value = "premium", keep_na = FALSE
      )
    )
  )

source_two <- set_source(
  as.tblist(librarian)
) |>
  add_step(
    step(
      filter(
        "range",
        id = "copies", dataset = "books",
        variable = "copies", range = c(-Inf, 5L)
      )
    )
  )

my_cohort <- cohort(source_one)
sum_up(my_cohort)
#> >> Step ID: 1 [pending]
#> -> Filter ID: author
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: books
#>      variable: author
#>      value: Dan Brown
#>      keep_na: TRUE
#> -> Filter ID: program
#>    Filter Type: discrete
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: borrowers
#>      variable: program
#>      value: premium
#>      keep_na: FALSE

update_source(my_cohort, source_two)
sum_up(my_cohort)
#> >> Step ID: 1 [pending]
#> -> Filter ID: copies
#>    Filter Type: range
#>    Filter Parameters:
#>      active: TRUE
#>      description: 
#>      domain: 
#>      dataset: books
#>      variable: copies
#>      range: -Inf, 5
#>      keep_na: TRUE
```

## Statistics and domain propagation

Two
[`cohort()`](https://r-world-devs.github.io/cohortBuilder/reference/create-cohort.md)
arguments control how much work the cohort does behind the scenes and
how filter domains evolve across steps.

### `compute_stats`

By default (`compute_stats = TRUE`) the cohort computes and stores
filter and data statistics after each step. These power
[`stat()`](https://r-world-devs.github.io/cohortBuilder/reference/stat.md),
[`attrition()`](https://r-world-devs.github.io/cohortBuilder/reference/attrition.md),
and the GUI summaries.

If you only need the filtered data (for example in a headless / batch
context), set `compute_stats = FALSE` to skip statistics computation:

``` r

librarian_source <- set_source(as.tblist(librarian))

fast_cohort <- librarian_source |>
  cohort(
    filter(
      "discrete",
      id = "author", dataset = "books",
      variable = "author", value = "Dan Brown"
    ),
    compute_stats = FALSE,
    run_flow = TRUE
  )

get_data(fast_cohort)$books
#> # A tibble: 2 × 6
#>   isbn          title             genre                  publisher author copies
#>   <chr>         <chr>             <chr>                  <chr>     <chr>   <int>
#> 1 0-385-50420-9 The Da Vinci Code Crime, Thriller & Adv… Transwor… Dan B…      7
#> 2 0-671-02735-2 Angels and Demons Crime, Thriller & Adv… Transwor… Dan B…      4
```

### `propagate_domains`

In multi-step filtering, a later step’s filter may still advertise its
full domain even though earlier steps have already narrowed the data.
The `propagate_domains` argument controls whether (and how) a filter’s
domain is restricted based on upstream steps:

- `"none"` (default) - domains are left untouched,
- `"filter"` - narrow from the previous step’s filter values,
- `"stats"` - narrow from stored statistics (requires
  `compute_stats = TRUE`),
- `"data"` - narrow by scanning the filtered data (the stats-free
  equivalent).

Domain propagation only adjusts filters that already declare an initial
`domain`. Below, step 1 keeps only Dan Brown’s books, so step 2’s
`genre` filter domain narrows to the genres that actually remain:

``` r

all_genres <- sort(collapse::funique(librarian$books$genre))

propagated_cohort <- librarian_source |>
  cohort(
    step(
      filter(
        "discrete",
        id = "author", dataset = "books",
        variable = "author", value = "Dan Brown"
      )
    ),
    step(
      filter(
        "discrete",
        id = "genre", dataset = "books",
        variable = "genre", domain = all_genres
      )
    ),
    propagate_domains = "data",
    run_flow = TRUE
  )

# Full domain vs. domain after step 1 narrowing
all_genres
#> [1] "Crime, Thriller & Adventure" "Food & Drink: General"      
#> [3] "General & Literary Fiction"  "Popular Science"            
#> [5] "Young Adult Fiction"
propagated_cohort$get_step("2")$filters[["genre"]]@domain
#> [1] "Crime, Thriller & Adventure"
```
