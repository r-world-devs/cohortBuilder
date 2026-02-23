# Specifying data relations with Binding Keys

When source consists of multiple datasets, binding keys allow to define
what relations occur between them. When binding keys are defined,
applying filtering on one dataset may result with updating (filtering)
the other ones.

To explain how binding keys work and how to define them we’ll be using
[`cohortBuilder::librarian`](https://r-world-devs.github.io/cohortBuilder/reference/librarian.md)
dataset:

``` r
str(librarian)
#> List of 4
#>  $ books    : tibble [17 × 6] (S3: tbl_df/tbl/data.frame)
#>   ..$ isbn     : chr [1:17] "0-385-50420-9" "0-7679-0817-1" "978-0-15-602943-8" "0-224-06252-2" ...
#>   ..$ title    : chr [1:17] "The Da Vinci Code" "A Short History of Nearly Everything" "The Time Traveler's Wife" "Atonement" ...
#>   ..$ genre    : chr [1:17] "Crime, Thriller & Adventure" "Popular Science" "General & Literary Fiction" "General & Literary Fiction" ...
#>   ..$ publisher: chr [1:17] "Transworld" "Transworld" "Random House" "Random House" ...
#>   ..$ author   : chr [1:17] "Dan Brown" "Bill Bryson" "Audrey Niffenegger" "Ian McEwan" ...
#>   ..$ copies   : int [1:17] 7 4 2 8 11 4 9 1 14 11 ...
#>  $ borrowers: tibble [20 × 6] (S3: tbl_df/tbl/data.frame)
#>   ..$ id          : chr [1:20] "000001" "000002" "000003" "000004" ...
#>   ..$ registered  : Date[1:20], format: "2001-06-09" "2002-08-10" ...
#>   ..$ address     : chr [1:20] "66 N. Evergreen Ave. Norristown, PA 19401" "8196 Windsor Road Muscatine, IA 52761" "6 Wood Lane Calumet City, IL 60409" "18 Nut Swamp Road Merrimack, NH 03054" ...
#>   ..$ name        : chr [1:20] "Mrs. Freddie Pouros DDS" "Ms. Jada Lesch" "Inga Dach" "Keyshawn Schaefer" ...
#>   ..$ phone_number: chr [1:20] "626-594-4729" "919-530-5272" "706-669-5694" "746-328-6598" ...
#>   ..$ program     : chr [1:20] "premium" "standard" NA "standard" ...
#>  $ issues   : tibble [50 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ id         : chr [1:50] "000001" "000002" "000003" "000004" ...
#>   ..$ borrower_id: chr [1:50] "000019" "000010" "000016" "000005" ...
#>   ..$ isbn       : chr [1:50] "0-676-97976-9" "978-0-7528-6053-4" "0-09-177373-3" "0-224-06252-2" ...
#>   ..$ date       : Date[1:50], format: "2015-03-17" "2008-09-13" ...
#>  $ returns  : tibble [30 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ id  : chr [1:30] "000001" "000003" "000004" "000005" ...
#>   ..$ date: Date[1:30], format: "2015-04-06" "2014-10-23" ...
```

Let’s say we want to get list of all the library members that borrowed a
selected book in 2016.

To start with, we define a Source storing `librarian` object, create
Cohort and configure required filters in it (we choose “Birdsong” book
as an example):

``` r
librarian_source <- set_source(
  as.tblist(librarian)
)

librarian_cohort <- librarian_source %>%
  cohort(
    step(
      filter(
        "discrete",
        id = "title", dataset = "books",
        variable = "title", value = "Birdsong"
      ),
      filter(
        "date_range",
        id = "issue_date", dataset = "issues",
        variable = "date", range = c(as.Date("2016-01-01"), as.Date("2016-12-31"))
      )
    )
  )
```

The above filters cover only part of our case scenario condition:

- filtering selected book,
- filtering all the issues from 2016.

Configuring filters for borrowers is impossible - we don’t know upfront
which issues were related to the selected book.

How can we overcome the issue?

## Classic approach

With the classic approach, we may iteractively extract each information
and extend cohort filters, for example we can define a new condition in
the next filtering step:

``` r
run(librarian_cohort)
selected_isbn <- get_data(librarian_cohort)$books$isbn
librarian_cohort %->%
  step(
    filter("discrete", id = "isbn", dataset = "issues", variable = "isbn", value = selected_isbn)
  ) %>%
  run(step_id = 2L)
```

Now `librarian_cohort` should store all the issues related to selected
book. For the final part we need to filter borrowers based on those
issues. We’ll do this filtering in the third step:

``` r
selected_borrower_id <- get_data(librarian_cohort)$issues$borrower_id
librarian_cohort %->%
  step(
    filter("discrete", id = "borr_id", dataset = "borrowers", variable = "id", value = selected_borrower_id)
  ) %>%
  run(step_id = 3L)
```

Resulting third-step data should contain desired information:

``` r
get_data(librarian_cohort)$borrowers
#> # A tibble: 2 × 6
#>   id     registered address                           name  phone_number program
#>   <chr>  <date>     <chr>                             <chr> <chr>        <chr>  
#> 1 000009 2007-07-19 9692 E. Fifth Dr. Waterbury, CT … Mari… 669-975-6908 standa…
#> 2 000017 2014-04-07 8501 Lawrence Rd. Terre Haute, I… Rand… 895-995-2326 premium
```

## Binding Keys

Now we’re going to get the same result with using binding-keys.

A single binding key describes relation of specified table, with the
other set of tables included in source.

**Note.** We relate here to tables, but this can be any other object to
which bindings abstraction is valid (i.e. relation between lists).

Let’s define relation between books and issues first and then we’ll
explain the syntax:

``` r
issue_books_bk <- bind_key(
  update = data_key(dataset = "issues", key = "isbn"),
  data_key(dataset = "books", key = "isbn")
)
```

Shortly we should understand this key as:

**Whenever table `books` is filtered, filter `issues` table as well (by
inner join on `isbn` of both tables)**

To make binding keys description easier, let’s name the table passed to
`update` parameter as **dependent** one, and tables defined at `...` as
**triggering** ones.

As we could see above, we define both dependent and triggering tables
using data keys (`data_key`). The first parameter of `data_key`,
`dataset` stores information about source object name whereas `key` is a
vector of keys used in join.

**Note.** We can define only one dependent table in each binding key,
but any number of triggering tables (keeping in mind the defined keys
are of the same length).

**Note.** Dependent table is updated if any of triggering tables
changes.

**Note.** The triggering table is considered to be **changed** when it
has at least active filter attached.

**Note.** The `dependent` table is marked as **changed** when updated
(so that can trigger some bindings even if no active filters were attach
to it). You may change this behavior by specifying `activate = FALSE` in
`bind_key`.

No we can define full list of binding keys solving our case. We wrap
multiple keys together using `bind_keys`:

``` r
case_bks <- bind_keys(
  bind_key(
    update = data_key(dataset = "issues", key = "isbn"),
    data_key(dataset = "books", key = "isbn")
  ),
  bind_key(
    update = data_key(dataset = "borrowers", key = "id"),
    data_key(dataset = "issues", key = "borrower_id")
  )
)
```

We define binding keys while creating source, so we need to:

``` r
librarian_source <- set_source(
  as.tblist(librarian),
  binding_keys = case_bks
)

librarian_cohort <- librarian_source %>%
  cohort(
    step(
      filter(
        "discrete",
        id = "title", dataset = "books",
        variable = "title", value = "Birdsong"
      ),
      filter(
        "date_range",
        id = "issue_date", dataset = "issues",
        variable = "date", range = c(as.Date("2016-01-01"), as.Date("2016-12-31"))
      )
    )
  )
```

Now:

``` r
run(librarian_cohort)
get_data(librarian_cohort)
#> $books
#> # A tibble: 1 × 6
#>   isbn          title    genre                      publisher    author   copies
#>   <chr>         <chr>    <chr>                      <chr>        <chr>     <int>
#> 1 0-09-177373-3 Birdsong General & Literary Fiction Random House Sebasti…      4
#> 
#> $borrowers
#> # A tibble: 2 × 6
#>   id     registered address                           name  phone_number program
#>   <chr>  <date>     <chr>                             <chr> <chr>        <chr>  
#> 1 000009 2007-07-19 9692 E. Fifth Dr. Waterbury, CT … Mari… 669-975-6908 standa…
#> 2 000017 2014-04-07 8501 Lawrence Rd. Terre Haute, I… Rand… 895-995-2326 premium
#> 
#> $issues
#> # A tibble: 2 × 4
#>   id     borrower_id isbn          date      
#>   <chr>  <chr>       <chr>         <date>    
#> 1 000009 000017      0-09-177373-3 2016-11-12
#> 2 000044 000009      0-09-177373-3 2016-11-20
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

returns desired result within a single step.
