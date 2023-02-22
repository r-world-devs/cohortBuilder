substitute_q <- function(x, env) {
  call <- substitute(substitute(y, env), list(y = x))
  eval(call)
}

pair_seq <- function(idxs) {

  if (length(idxs) == 0) {
    return(integer(0))
  }
  idxs <- sort(idxs)
  sequence <- c()
  for (idx in seq(1, length(idxs), by = 2)) {
    sequence <- c(sequence, seq(idxs[idx], idxs[idx + 1], by = 1))
  }
  return(sequence)
}

parse_func_expr <- function(func) {

  if (is.null(func)) {
    return(quote({}))
  }
  func_body <- utils::capture.output(body(func))
  n_lines <- length(func_body)
  func_body[n_lines - 1] <- glue::glue("data_object <- {func_body[n_lines - 1]}")
  func_expr <- parse(text = func_body)[[1]]

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
  closing_idx <- rev(which("}" == func_expr))[1]
  return(
    parse(text = func_expr[1:closing_idx])[[1]]
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
  code_eval_expr <- parse(text = c("{", filter_expr[code_eval_idx], "}"))[[1]]
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

  if (length(sub_expr_idx) == 0) {
    return(str2lang("{}"))
  }

  filter_expr <- parse(text = c("{", filter_expr[sub_expr_idx], "}"))[[1]]
  sub_vars <- substitute_q(filter_expr, vars_env)
  sub_syms <-   rlang::inject((!!rlang::expr)(!!sub_vars), filter_env)
  return(sub_syms)
}

combine_expressions <- function(expressions_list) {
  expressions_list <- lapply(expressions_list, function(x) {
    if (x[[1]] == as.symbol("{")) {
      return(as.list(x)[-1])
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
  if (expr[[1]] == as.symbol("{")) {
    if (identical(expr[[2]][[2]], after) && expr[[2]][[1]] == as.symbol("%>%")) {
      expr[[2]] <- expr[[2]][[3]]
    }
    else {
      expr[[2]][[2]] <- exclude_first_pipe(expr[[2]][[2]], after)
    }
  } else {
    if (identical(expr[[2]], after) && expr[[1]] == as.symbol("%>%")) {
      expr <- expr[[3]]
    }
    else {
      expr[[2]] <- exclude_first_pipe(expr[[2]], after)
    }
  }
  return(expr)
}

exclude_reassignment <- function(expr, along_with = c("left", "both")) {
  along_with <- match.arg(along_with)
  if (expr[[1]] == as.symbol("{")) {
    to_exclude <- expr[[2]][[2]]
    if (expr[[2]][[1]] == as.symbol("<-")) {
      expr[[2]] <- expr[[2]][[3]]
      if (along_with == "both") {
        expr[[2]] <- exclude_first_pipe(expr[[2]], to_exclude)
      }
    } else {
      warning("First line of expression is not a reassignment.")
    }
  } else {
    to_exclude <- expr[[2]]
    if (expr[[1]] == as.symbol("<-")) {
      expr <- expr[[3]]
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
  if (expr[[1]] == as.symbol("{")) {
     return(expr[[2]])
  }
  return(expr)
}

pipe_reassignment <- function(expr_l, expr_r) {
  rlang::expr(!!expr1 %>% expr_r)
}

nos <- rlang::expr({
  a %>% sum()
  b <- 1
})

pipe_filtering <- function(filtering_exprs) {
  n_exprs <- length(filtering_exprs)
  if (n_exprs <= 1) {
    return(filtering_exprs)
  }
  if (n_exprs > 1) {
    for (expr_id in seq_along(filtering_exprs)) {
      if (expr_id > 1) {
        filtering_exprs[[expr_id]] <- filtering_exprs[[expr_id]] %>%
          exclude_reassignment(along_with = "both")
      }
      if (expr_id < n_exprs) {
        filtering_exprs[[expr_id]] <- filtering_exprs[[expr_id]] %>%
          take_first_line()
      }
      if (expr_id == 1) {
        res_expr <- exclude_reassignment(filtering_exprs[[expr_id]], along_with = "left")
      } else {
        if (filtering_exprs[[expr_id]][[1]] == as.symbol("{")) {
          res_expr <- rlang::expr(
            !!res_expr %>%
              !!filtering_exprs[[expr_id]][[2]]
          )
          for (i in setdiff(seq_along(filtering_exprs[[expr_id]]), 1:2)) {
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
  assignment <- rlang::expr(!!filtering_exprs[[1]][[2]] <- x)
  if (res_expr[[1]] == as.symbol("{")) {
    res_expr[[2]] <- substitute_q(assignment, list(x = res_expr[[2]]))
  } else {
    res_expr <- substitute_q(assignment, list(x = res_expr))
  }

  return(list(res_expr))
}

if_null_default_list <- function(x, y) {
  if (is.null(y[[1]])) {
    return(x)
  }
  return(y)
}

flatten_listcol <- function(x) {
  if (is.null(x[[1]])) {
    return(NA)
  }
  return(x[[1]])
}

pipe_all_filters <- function(expr_df) {

  if (!"dataset" %in% colnames(expr_df)) {
    expr_df <- expr_df %>% dplyr::mutate(dataset = NA)
  }

  expr_df <- expr_df %>% dplyr::mutate(dataset = purrr::map_chr(dataset, flatten_listcol))
  filtering_expr_df <- expr_df %>% dplyr::filter(type == "filtering")

  if (nrow(filtering_expr_df) == 0) {
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
    dplyr::distinct()
}
