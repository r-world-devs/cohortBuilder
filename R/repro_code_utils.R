#' `substitute()` on a quoted expression
#'
#' Performs variable substitution on an already-quoted expression `x` using the
#' bindings in `env` (a programmatic alternative to `substitute()`).
#'
#' @param x A quoted expression.
#' @param env A named list of substitutions.
#' @return The expression with substitutions applied.
#' @noRd
substitute_q <- function(x, env) {
  call <- substitute(substitute(y, env), list(y = x))
  eval(call)
}


#' Turn a source method's body into an inlined expression
#'
#' Captures a function's body, assigns its final value to `data_object`, and
#' substitutes the function's closure/default arguments so it can be embedded in
#' reproducible code.
#'
#' @param func A function (or `NULL`).
#' @return A call expression, or an empty block when `func` is `NULL`.
#' @noRd
parse_func_expr <- function(func) {

  if (is.null(func)) {
    return(quote({}))
  }
  func_body <- utils::capture.output(body(func))
  n_lines <- length(func_body)
  func_body[n_lines - 1L] <- glue::glue("data_object <- {func_body[n_lines - 1]}")
  func_expr <- parse(text = func_body)[[1L]]

  substitute_q(
    func_expr,
    append(as.list(environment(func)), purrr::keep(formals(func), Negate(is.symbol)))
  )
}

#' Render a function definition as an assignment expression
#'
#' Produces `name <- function(...) {...}` from a function object, trimming any
#' trailing namespace bytecode/environment lines.
#'
#' @param func A function (or `NULL`).
#' @param name Name to assign the function to.
#' @return An assignment call, or an empty block when `func` is `NULL`.
#' @noRd
func_to_expr <- function(func, name) {
  if (is.null(func)) {
    return(quote({}))
  }

  func_expr <- c(paste(name, "<-"), utils::capture.output(func))
  # in case function comes from namespace
  closing_idx <- rev(which(grepl("}$", func_expr, perl = TRUE)))[1L]
  return(
    parse(text = func_expr[1L:closing_idx])[[1L]]
  )
}

#' Render an S3 method definition as an expression
#'
#' Looks up `name.namespace` and renders it as a function-assignment expression
#' for inclusion in reproducible code.
#'
#' @param name Generic name (e.g. `".init_step"`).
#' @param namespace Source-type suffix (e.g. `"tblist"`).
#' @return An expression, or `NULL` when the method is not found.
#' @noRd
method_to_expr <- function(name, namespace) {
  method <- .get_method(paste0(name, ".", namespace))
  if (is.null(method)) {
    return(NULL)
  }
  substitute(
    {fun_expr},
    list(
      fun_expr = func_to_expr(
        method,
        name
      )
    )
  )
}

#' Build an assignment expression `name <- value`
#'
#' @param name Symbol/name to assign to.
#' @param value Value expression.
#' @return An assignment call.
#' @noRd
assign_expr <- function(name, value) {
  substitute(
    {value_name <- value_content},
    list(
      value_name = name,
      value_content = value
    )
  )
}


#' Merge several expressions into a single `{ }` block
#'
#' Flattens any top-level `{ }` blocks among the inputs so the result is one
#' brace-wrapped sequence of statements.
#'
#' @param expressions_list A list of call expressions.
#' @return A single `{ }` call combining all statements.
#' @noRd
combine_expressions <- function(expressions_list) {
  expressions_list <- lapply(expressions_list, function(x) {
    if (x[[1L]] == as.symbol("{")) {
      return(as.list(x)[-1L])
    } else {
      return(x)
    }
  })
  expressions_list <- do.call("c", expressions_list, quote = TRUE)
  as.call(c(as.symbol("{"), expressions_list))
}

#' Build the source-construction expression for reproducible code
#'
#' Combines the source's `source_code` (or a default `list(dtconn = ...)`) with
#' the source type's `.init_step` body.
#'
#' @param source_type Source type string (e.g. `"tblist"`).
#' @param public,private Cohort R6 public/private environments.
#' @return A `{ }` expression that reconstructs `source` and the step data.
#' @noRd
get_source_expr <- function(source_type, public, private) {
  source_expr <- if (!is.null(private$source$source_code)) {
    private$source$source_code
  } else {
    substitute(source <- list(dtconn = x), list(x = attr(private$source$dtconn, "call")))
  }

  init_step_expr <- parse_func_expr(
    .get_method(paste0(".init_step", ".", source_type))
  )
  return(combine_expressions(list(source_expr, init_step_expr)))
}

#' Wrap an expression with its reproducible-code metadata
#'
#' Builds a one-row tibble tagging an expression with its `action`, `step`, and
#' any extra columns (e.g. `dataset`), used to assemble the final code.
#'
#' @param action Action label (e.g. `"filtering"`, `"pre_filtering"`).
#' @param expr The expression to wrap.
#' @param step Step id (or `NA`).
#' @param ... Extra metadata columns.
#' @return A list containing a single tibble row.
#' @noRd
type_expr <- function(action, expr, step = NA, ...) {
  args <- rlang::dots_list(...)
  args <- args |>
    purrr::modify(list)
  base_data <- tibble::tibble(action = action, expr = list(expr), step = step)
  if (!length(args)) {
    return(list(base_data))
  }
  list(dplyr::bind_cols(
    base_data,
    tibble::as_tibble(args)
  ))
}

#' Remove the first piped/argument occurrence of a sub-expression
#'
#' Strips `after` where it appears as the left operand of the first `|>` (or as
#' the first call argument), used to splice consecutive filtering steps into one
#' pipe chain.
#'
#' @param expr An expression to rewrite.
#' @param after The sub-expression to drop.
#' @return The rewritten expression.
#' @noRd
exclude_first_pipe <- function(expr, after) {
  if (is.symbol(expr) || is.atomic(expr)) {
    return(expr)
  }
  if (expr[[1L]] == as.symbol("{")) {
    if (identical(expr[[2L]][[2L]], after) && expr[[2L]][[1L]] == as.symbol("|>")) {
      expr[[2L]] <- expr[[2L]][[3L]]
    } else {
      expr[[2L]][[2L]] <- exclude_first_pipe(expr[[2L]][[2L]], after)
    }
  } else if (expr[[1L]] == as.symbol("|>")) {
    if (identical(expr[[2L]], after)) {
      expr <- expr[[3L]]
    } else {
      expr[[2L]] <- exclude_first_pipe(expr[[2L]], after)
    }
  } else if (identical(expr[[2L]], after)) {
    # Function call with `after` as first argument — remove it
    expr[[2L]] <- NULL
  }
  return(expr)
}

#' Strip the leading `x <- ` from an expression
#'
#' Removes the reassignment on the first statement, optionally also dropping the
#' first pipe of the right-hand side (`along_with = "both"`).
#'
#' @param expr An expression whose first statement is an assignment.
#' @param along_with `"left"` to drop only the assignment, `"both"` to also drop
#'   the first pipe of the value.
#' @return The rewritten expression.
#' @noRd
exclude_reassignment <- function(expr, along_with = c("left", "both")) {
  along_with <- match.arg(along_with)
  if (expr[[1L]] == as.symbol("{")) {
    to_exclude <- expr[[2L]][[2L]]
    if (expr[[2L]][[1L]] == as.symbol("<-")) {
      expr[[2L]] <- expr[[2L]][[3L]]
      if (along_with == "both") {
        expr[[2L]] <- exclude_first_pipe(expr[[2L]], to_exclude)
      }
    } else {
      warning("First line of expression is not a reassignment.")
    }
  } else {
    to_exclude <- expr[[2L]]
    if (expr[[1L]] == as.symbol("<-")) {
      expr <- expr[[3L]]
      if (along_with == "both") {
        expr <- exclude_first_pipe(expr, to_exclude)
      }
    } else {
      warning("First line of expression is not a reassignment.")
    }
  }
  return(expr)
}

#' Return the first statement of a `{ }` block
#'
#' @param expr An expression, possibly a brace block.
#' @return The first statement (or `expr` itself when not a block).
#' @noRd
take_first_line <- function(expr) {
  if (expr[[1L]] == as.symbol("{")) {
    return(expr[[2L]])
  }
  return(expr)
}

#' Insert the left operand as the first argument of the right call
#'
#' Reproduces native-pipe semantics (`x |> f(y)` parses to `f(x, y)`)
#' programmatically.
#'
#' @param expr_l Left-hand expression.
#' @param expr_r Right-hand call.
#' @return The combined call.
#' @noRd
pipe_reassignment <- function(expr_l, expr_r) {
  # Native pipe |> is syntactic: x |> f(y) parses to f(x, y).
  # Reproduce this by inserting expr_l as the first argument of expr_r.
  as.call(append(as.list(expr_r), list(expr_l), after = 1L))
}

#' Chain per-filter expressions into a single pipe assignment
#'
#' Combines the filtering expressions for one dataset/step into one
#' `dataset <- dataset |> ... |> ...` pipe chain.
#'
#' @param filtering_exprs A list of per-filter expressions.
#' @return A list containing the single combined expression.
#' @noRd
pipe_filtering <- function(filtering_exprs) {
  n_exprs <- length(filtering_exprs)
  if (n_exprs <= 1L) {
    return(filtering_exprs)
  }
  if (n_exprs > 1L) {
    for (expr_id in seq_along(filtering_exprs)) {
      if (expr_id > 1L) {
        filtering_exprs[[expr_id]] <- filtering_exprs[[expr_id]] |>
          exclude_reassignment(along_with = "both")
      }
      if (expr_id < n_exprs) {
        filtering_exprs[[expr_id]] <- filtering_exprs[[expr_id]] |>
          take_first_line()
      }
      if (expr_id == 1L) {
        res_expr <- exclude_reassignment(filtering_exprs[[expr_id]], along_with = "left")
      } else {
        if (filtering_exprs[[expr_id]][[1L]] == as.symbol("{")) {
          res_expr <- pipe_reassignment(res_expr, filtering_exprs[[expr_id]][[2L]])
          for (i in setdiff(seq_along(filtering_exprs[[expr_id]]), 1L:2L)) {
            res_expr <- rlang::expr({
              !!res_expr
              !!filtering_exprs[[expr_id]][[i]]
            })
          }
        } else {
          res_expr <- pipe_reassignment(res_expr, filtering_exprs[[expr_id]])
        }
      }
    }
  }
  assignment <- rlang::expr(!!filtering_exprs[[1L]][[2L]] <- x)
  if (res_expr[[1L]] == as.symbol("{")) {
    res_expr[[2L]] <- substitute_q(assignment, list(x = res_expr[[2L]]))
  } else {
    res_expr <- substitute_q(assignment, list(x = res_expr))
  }

  return(list(res_expr))
}

#' Fall back to `x` when the first element of `y` is `NULL`
#'
#' @param x Default value.
#' @param y Candidate value (a list).
#' @return `x` when `y[[1]]` is `NULL`, otherwise `y`.
#' @noRd
if_null_default_list <- function(x, y) {
  if (is.null(y[[1L]])) {
    return(x)
  }
  return(y)
}

#' Unwrap a single-element list column, mapping `NULL` to `NA`
#'
#' @param x A length-one list.
#' @return The unwrapped element, or `NA` when it is `NULL`.
#' @noRd
flatten_listcol <- function(x) {
  if (is.null(x[[1L]])) {
    return(NA)
  }
  return(x[[1L]])
}

#' Collapse all filtering expressions into per-group pipe chains
#'
#' Groups the expression tibble by action/step/dataset, pipes each filtering
#' group via [pipe_filtering()], and returns the `action`/`expr` columns.
#'
#' @param expr_df A tibble of tagged expressions (see [type_expr()]).
#' @return A tibble with `action` and combined `expr` columns.
#' @noRd
pipe_all_filters <- function(expr_df) {

  if (!"dataset" %in% colnames(expr_df)) {
    expr_df <- expr_df |> dplyr::mutate(dataset = NA)
  }

  expr_df <- expr_df |> dplyr::mutate(dataset = purrr::map_chr(dataset, flatten_listcol))
  filtering_expr_df <- expr_df |> dplyr::filter(action == "filtering")

  if (nrow(filtering_expr_df) == 0L) {
    return(dplyr::select(expr_df, action, expr))
  }

  expr_df |> dplyr::left_join(
    filtering_expr_df |>
      dplyr::group_by(action, step, dataset) |>
      dplyr::summarise(new_expr = pipe_filtering(expr)) |>
      dplyr::ungroup(),
    by = c("action", "step", "dataset")
  ) |>
    dplyr::mutate(expr = purrr::map2(expr, new_expr, if_null_default_list)) |>
    dplyr::select(action, expr) |>
    # collapse::funique not support nested tables with custom values
    dplyr::distinct()
}
