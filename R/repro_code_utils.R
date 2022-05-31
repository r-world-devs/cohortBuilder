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
    return(quote({}))
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
  sub_vars <- substitute_q(filter_expr, filter_env)
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
