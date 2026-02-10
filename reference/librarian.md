# Sample of library database

A list containing four data frames reflecting library management
database.

## Usage

``` r
librarian
```

## Format

A list of four data frames:

**books** - books on store

- `isbn`:

  book ISBN number

- `title`:

  book title

- `genre`:

  comma separated book genre

- `publisher`:

  name of book publisher

- `author`:

  name of book author

- `copies`:

  total number of book copies on store

**borrowers** - registered library members

- `id`:

  member unique id

- `registered`:

  date the member joined library

- `address`:

  member address

- `name`:

  full member name

- `phone_number`:

  member phone number

- `program`:

  membership program type (standard, premium or vip)

**issues** - borrowed books events

- `id`:

  unique event id

- `borrower_id`:

  id of the member that borrowed the book

- `isbn`:

  is of the borrowed book

- `date`:

  date of borrow event

**returns** - returned books events

- `id`:

  event id equal to borrow issue id

- `date`:

  date of return event
