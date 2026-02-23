substitute_q <- function(x, env) {
  call <- substitute(substitute(y, env), list(y = x))
  eval(call)
}

pair_seq <- function(idxs) {

  if (length(idxs) == 0L) {
    return(integer(0L))
  }

  if (!identical(length(idxs) %% 2L, 0L)) {
    stop("The lenght of idxs is not even number")
  }

  idxs <- sort(idxs)
  sequence <- c()
  for (idx in seq(1L, length(idxs), by = 2L)) {
    sequence <- c(sequence, seq(idxs[idx], idxs[idx + 1L], by = 1L))
  }
  return(sequence)
}

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

assign_expr <- function(name, value) {
  substitute(
    {value_name <- value_content},
    list(
      value_name = name,
      value_content = value
    )
  )
}

parse_filter_expr <- function(filter) {
  filter_env <- environment(filter$filter_data)
  keep_na <- identical(filter_env$keep_na, TRUE)
  selected_value <- filter_env[[filter$input_param]]
  filter_expr <- utils::capture.output(filter$filter_data)
  vars_env <- as.list(filter_env)

  code_eval_idx <- sort(c(
    pair_seq(grep("# code eval ", filter_expr, fixed = TRUE)),
    grep("# code eval$", filter_expr)
  ))
  code_eval_expr <- parse(text = c("{", filter_expr[code_eval_idx], "}"))[[1L]]
  rlang::eval_bare(code_eval_expr, filter_env)

  keep_na_ind <- if (keep_na) "keep_na" else "!keep_na"
  value_na_ind <- if (identical(selected_value, NA)) "value_na" else "!value_na"
  expr_ind <- glue::glue(" {keep_na_ind} {value_na_ind} ")

  expr_idx <- grep(expr_ind, filter_expr, fixed = TRUE)
  code_include_idx <- grep("# code include ", filter_expr, fixed = TRUE)
  sub_expr_idx <- pair_seq(c(expr_idx, code_include_idx))

  sub_expr_idx <- sort(c(
    sub_expr_idx,
    grep("# code include$", filter_expr)
  ))

  if (length(sub_expr_idx) == 0L) {
    return(str2lang("{}"))
  }

  filter_expr <- parse(text = c("{", filter_expr[sub_expr_idx], "}"))[[1L]]
  sub_vars <- substitute_q(filter_expr, vars_env)
  sub_syms <-   rlang::inject((!!rlang::expr)(!!sub_vars), filter_env)
  return(sub_syms)
}

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

type_expr <- function(type, expr, step = NA, ...) {
  args <- rlang::dots_list(...)
  args <- args %>%
    purrr::modify(list)
  base_data <- tibble::tibble(type = type, expr = list(expr), step = step)
  if (!length(args)) {
    return(list(base_data))
  }
  list(dplyr::bind_cols(
    base_data,
    tibble::as_tibble(args)
  ))
}

exclude_first_pipe <- function(expr, after) {
  if (expr[[1L]] == as.symbol("{")) {
    if (identical(expr[[2L]][[2L]], after) && expr[[2L]][[1L]] == as.symbol("%>%")) {
      expr[[2L]] <- expr[[2L]][[3L]]
    } else {
      expr[[2L]][[2L]] <- exclude_first_pipe(expr[[2L]][[2L]], after)
    }
  } else {
    if (identical(expr[[2L]], after) && expr[[1L]] == as.symbol("%>%")) {
      expr <- expr[[3L]]
    } else {
      expr[[2L]] <- exclude_first_pipe(expr[[2L]], after)
    }
  }
  return(expr)
}

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

take_first_line <- function(expr) {
  if (expr[[1L]] == as.symbol("{")) {
    return(expr[[2L]])
  }
  return(expr)
}

pipe_reassignment <- function(expr_l, expr_r) {
  rlang::expr(!!expr1 %>% expr_r)
}

nos <- rlang::expr({
  a %>% sum()
  b <- 1L
})

pipe_filtering <- function(filtering_exprs) {
  n_exprs <- length(filtering_exprs)
  if (n_exprs <= 1L) {
    return(filtering_exprs)
  }
  if (n_exprs > 1L) {
    for (expr_id in seq_along(filtering_exprs)) {
      if (expr_id > 1L) {
        filtering_exprs[[expr_id]] <- filtering_exprs[[expr_id]] %>%
          exclude_reassignment(along_with = "both")
      }
      if (expr_id < n_exprs) {
        filtering_exprs[[expr_id]] <- filtering_exprs[[expr_id]] %>%
          take_first_line()
      }
      if (expr_id == 1L) {
        res_expr <- exclude_reassignment(filtering_exprs[[expr_id]], along_with = "left")
      } else {
        if (filtering_exprs[[expr_id]][[1L]] == as.symbol("{")) {
          res_expr <- rlang::expr(
            !!res_expr %>%
              !!filtering_exprs[[expr_id]][[2L]]
          )
          for (i in setdiff(seq_along(filtering_exprs[[expr_id]]), 1L:2L)) {
            res_expr <- rlang::expr({
              !!res_expr
              !!filtering_exprs[[expr_id]][[i]]
            })
          }
        } else {
          res_expr <- rlang::expr(
            !!res_expr %>%
              !!filtering_exprs[[expr_id]]
          )
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

if_null_default_list <- function(x, y) {
  if (is.null(y[[1L]])) {
    return(x)
  }
  return(y)
}

flatten_listcol <- function(x) {
  if (is.null(x[[1L]])) {
    return(NA)
  }
  return(x[[1L]])
}

pipe_all_filters <- function(expr_df) {

  if (!"dataset" %in% colnames(expr_df)) {
    expr_df <- expr_df %>% dplyr::mutate(dataset = NA)
  }

  expr_df <- expr_df %>% dplyr::mutate(dataset = purrr::map_chr(dataset, flatten_listcol))
  filtering_expr_df <- expr_df %>% dplyr::filter(type == "filtering")

  if (nrow(filtering_expr_df) == 0L) {
    return(dplyr::select(expr_df, type, expr))
  }

  expr_df %>% dplyr::left_join(
    filtering_expr_df %>%
      dplyr::group_by(type, step, dataset) %>%
      dplyr::summarise(new_expr = pipe_filtering(expr)) %>%
      dplyr::ungroup(),
    by = c("type", "step", "dataset")
  ) %>%
    dplyr::mutate(expr = purrr::map2(expr, new_expr, if_null_default_list)) %>%
    dplyr::select(type, expr) %>%
    # collapse::funique not support nested tables with custom values
    dplyr::distinct()
}
