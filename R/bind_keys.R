#' Define Source datasets primary keys
#'
#' Primary keys can be defined as `primary_keys` parameter of \link{set_source} method.
#' Currently, primary keys are used only to show keys information in attrition plot (See \link{attrition}).
#'
#' @examples
#' primary_keys(
#'   data_key('books', 'book_id'),
#'   data_key('borrowed', c('user_id', 'books_id', 'date'))
#' )
#'
#' @param ... Data keys describing tables primary keys.
#' @return List of class `primary_keys` storing \link{data_key}s objects.
#'
#' @export
primary_keys <- function(...) {
  structure(
    list(...),
    class = "primary_keys"
  )
}

#' Describe data relations with binding keys
#'
#' @description
#'
#' When source consists of multiple datasets, binding keys allow to define what
#' relations occur between them.
#' When binding keys are defined, applying filtering on one dataset may result with
#' updating (filtering) the other ones.
#'
#' For example having two tables in Source:
#' `book(book_id, author_id, title)`
#' `authors(author_id, name, surname)`
#' if we filter `authors` table, we way want to return only books for the selected authors.
#'
#' With binding keys you could achieve it by providing `binding_keys` parameter for Source
#' as below:
#' \preformatted{
#'   binding_keys = bind_keys(
#'     bind_key(
#'       update = data_key('books', 'author_id'),
#'       data_key('authors', 'author_id')
#'     )
#'   )
#' }
#'
#' Or if we want to have two-way relation, just define another binding key:
#' \preformatted{
#'   binding_keys = bind_keys(
#'     bind_key(
#'       update = data_key('books', 'author_id'),
#'       data_key('authors', 'author_id')
#'     ),
#'     bind_key(
#'       update = data_key('authors', 'author_id'),
#'       data_key('books', 'author_id')
#'     )
#'   )
#' }
#'
#' As a result, whenever `books` or `authors` is filtered, the other table will be updated as well.
#'
#' In order to understand binding keys concept we need to describe the following functions:
#' \itemize{
#'   \item{\link{data_key} - Defines which table column should be used to describe relation.}
#'   \item{\code{bind_key} - Defines what relation occur between datasets.}
#'   \item{\code{bind_keys} - If needed, allows to define more than one relation.}
#' }
#'
#' - `data_key` - requires to provide two parameters:
#' \itemize{
#'   \item{\code{dataset} - Name of the dataset existing in Source.}
#'   \item{\code{key} - Single character string or vector storing column names that are keys, which should be used to describe relation.}
#' }
#' For example `data_key('books', 'author_id')`.
#'
#' - `bind_key` - requires to provide two obligatory parameters
#' \itemize{
#'   \item{\code{update} - Data key describing which table should be updated.}
#'   \item{\code{...} - \strong{Triggering data keys}. One or more data keys describing on which dataset(s) the one in `update` is dependent.}
#' }
#' The output of `bind_key` function is named \strong{binding key}.
#' `bind_key` offers two extra parameters `post` and `activate`.
#' See below to learn how these parameters affect the final result.
#'
#' - `bind_keys` - takes only binding keys as parameters
#' The function is used to define `binding_keys` parameter of Source.
#' Whenever you define a single or more binding keys wrap them with `bind_keys`.
#'
#' It's worth to mention that binding key describes inner-join like relation.
#' That means the updated table's key is intersection of its key and keys of
#' remaining tables defined in binding key.
#'
#' Another important note is that binding keys order matters - binding is performed
#' sequentially, taking into account returned data from the previous bindings.
#'
#' You may achieve more flexibility with two parameters:
#' \itemize{
#'   \item{\code{activate}}
#'   \item{\code{post}}
#' }
#'
#' \strong{Active tables and `activate` parameter}
#'
#' We name a table `active` that is attached to at least one active filter (in a step).
#'
#' When having defined binding key, e.g.
#' \preformatted{
#'   bind_key(
#'     update = data_key('books', 'author_id'),
#'     data_key('authors', 'author_id')
#'   )
#' }
#' the key is taken into account only when at least one triggering table is active.
#' So in the above example binding key will update `books` only when `authors` was
#' filtered (more precisely when any filter attached to `authors` is active).
#'
#' The `activate = TRUE` parameter setup, lets us to decide whether `update` table
#' should be marked as active as well when the binding finish.
#' This allows to build dependency chains between table.
#'
#' Let's explain this in the below example. Having defined another table in Source
#' `borrowed(book_id, user_id, date)` and binding key:
#' \preformatted{
#'   bind_keys(
#'     bind_key(
#'       update = data_key('books', 'book_id'),
#'       data_key('borrowed', 'book_id')
#'     ),
#'     bind_key(
#'       update = data_key('authors', 'author_id'),
#'       data_key('books', 'author_id')
#'     )
#'   )
#' }
#' Let's consider the case when table `borrowed` is active, `books` is not.
#' What happens during the binding process:
#' 1. Based on the first binding key, active `borrowed` triggers this one.
#' 2. As a result `books` is modified.
#'
#' What should happen with the second binding key.
#' We have two options:
#' 1. `books` could be marked as active as well so it triggers the second key.
#' 2. `books` could remain inactive so the second key is not triggered.
#' It will be triggered only when `books` is directly filtered (activated).
#'
#' You may choose between 1 and 2 with `activate = TRUE` (the default)  and `activate = FALSE`
#' respectively.
#'
#' So in the above example (because `activate = TRUE` by default) the authors table
#' will also be modified by the second binding key.
#'
#' To turn off this behavior we just need to:
#' \preformatted{
#'   bind_keys(
#'     bind_key(
#'       update = data_key('books', 'book_id'),
#'       data_key('borrowed', 'book_id'),
#'       activate = TRUE
#'     ),
#'     bind_key(
#'       update = data_key('authors', 'author_id'),
#'       data_key('books', 'author_id')
#'     )
#'   )
#' }
#'
#' \strong{Bind filtered on unfiltered data - `post` parameter}
#'
#' Let's tart with the below binding key example:
#' \preformatted{
#'   bind_keys(
#'     bind_key(
#'       update = data_key('authors', 'author_id'),
#'       data_key('books', 'author_id')
#'     )
#'   )
#' }
#'
#' Let's assume `authors` table is filtered and we apply filtering for `books` table.
#' We may want to achieve one of the two results:
#' 1. `authors` filters should be taken into account while binding.
#' 2. we should take unfiltered `authors` an apply binding based on `books` choices.
#'
#' We can achieve 1 and 2 with defining `post = TRUE` (the default) and `post = FALSE` respectively.
#'
#' So the following setup:
#' \preformatted{
#'   bind_keys(
#'     bind_key(
#'       update = data_key('authors', 'author_id'),
#'       data_key('books', 'author_id'),
#'       post = FALSE
#'     )
#'   )
#' }
#'
#' Whenever `books` is changed will result with filtering only the authors that written selected books -
#' no extra `authors` filters will be applied.
#'
#' There might be the situation when table was already bound but there is another
#' one binding key to be executed on the same table.
#'
#' In this case `post = FALSE` case will remain the same - unfiltered table will be taken.
#' More to that filtering and previous binding related to this table will be ignored.
#' In case of `post = TRUE` the previously bound table will be updated.
#'
#' @name binding-keys

#' @rdname binding-keys
#' @param ... In case of `bind_keys`, binding keys created with `bind_key`.
#'   In case of `bind_key`, data keys describing triggering tables.
#' @return List of class `bind_keys` storing `bind_key` class objects (`bind_keys`) or
#'   `bind_key` class list (`bind_key`).
#'
#' @export
bind_keys <- function(...) {
  structure(
    list(...),
    class = "bind_keys"
  )
}

#' @rdname binding-keys
#'
#' @param update Data key describing table to update.
#' @param post Update filtered or unfiltered table.
#' @param activate Mark bound table as active.
#' @export
bind_key <- function(update, ..., post = TRUE, activate = TRUE) {

  data_keys <- list(...)
  data_keys_names <- purrr::map_chr(data_keys, "dataset")
  binding_key <- structure(
    list(update = update, data_keys = data_keys, post = post, activate = activate),
    class = "bind_key"
  )
  names(binding_key$data_keys) <- data_keys_names

  return(binding_key)
}

#' Define Source dataset key
#'
#' Data keys are used to define \link{primary_keys} and \link{binding-keys}.
#'
#' @param dataset Name of the dataset included in Source.
#' @param key Character or character vector storing column names to be used as table keys.
#' @return `data_key` class list of two objects: `dataset` and `key` storing name and vector of
#'     data key names respectively.
#'
#' @export
data_key <- function(dataset, key) {
  structure(
    list(dataset = dataset, key = key),
    class = "data_key"
  )
}

#' @rdname source-layer
#' @export
.run_binding <- function(source, ...) {
  UseMethod(".run_binding", source)
}

#' @rdname source-layer
#' @param binding_key Binding key describing currently processed relation.
#' @param data_object_pre Object storing unfiltered data in the current step (previous step result).
#' @param data_object_post Object storing current data (including active filtering and previously done bindings).
#' @export
.run_binding.default <- function(source, binding_key, data_object_pre, data_object_post, ...) {
  return(data_object_post)
}
