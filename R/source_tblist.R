#' Create in memory tables connection
#'
#' Create data connection as a list of loaded data frames.
#' The object should be used as `dtconn` argument of \link{set_source}.
#'
#' @examples
#' str(tblist(mtcars))
#' str(tblist(mtcars, iris))
#' str(tblist(MT = mtcars, IR = iris))
#' str(tblist(mtcars, iris, names = c("MT", "IR")))
#'
#' @param ... Optionally named data frames.
#' @param names A character vector describing provided tables names.
#'   If missing names are constructed based on provided tables objects.
#' @param .class The extra (highest priority) class added to the resulting object.
#'   Having the extra class defined, enables to implement custom S3 methods for the object
#'   having higher priority over the existing methods.
#'   Especially useful if you want to change the built-in method behavior.
#' @return Object of class 'tblist' being a named list of data frames.
#' @export
tblist <- function(..., names, .class = NULL) {
  tables <- rlang::dots_list(..., .named = TRUE)
  out_class <- c(.class, "tblist")

  tb_call <- sys.call(1L)
  if (purrr::every(tables, is.data.frame)) {
    if (!missing(names)) {
      if (length(tables) != length(names)) {
        stop(glue::glue(
          "{sQuote('tables')} should be of same length as {sQuote('names')}"
        ))
      }
      names(tables) <- names
    }
    return(
      structure(tables, class = out_class)
    )
  }

  if (inherits(tables[[1L]], "list") && length(tables) == 1L) {
    if (!missing(names)) {
      if (length(tables[[1L]]) != length(names)) {
        stop(glue::glue(
          "{sQuote('tables')} should be of same length as {sQuote('names')}"
        ))
      }
      return(
        structure(
          stats::setNames(tables[[1L]], names),
          class = out_class
        )
      )
    } else {
      return(
        structure(tables, class = out_class)
      )
    }
  }
  stop("Please provide a data.frame or list of data.frames to create 'tblist' object.")
}

#' @rdname tblist
#' @param x an R object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
as.tblist <- function(x, ..., .class = NULL) {
  UseMethod("as.tblist", x)
}

#' @export
as.tblist.data.frame <- function(x, names, ..., .class = NULL) {
  tblist(x, names = names, .class = .class)
}

#' @export
as.tblist.list <- function(x, names, ..., .class = NULL) {
  tblist(!!!x, names = names, .class = .class)
}

#' @rdname set_source
#' @export
set_source.tblist <- function(dtconn, primary_keys = NULL, binding_keys = NULL,
                              source_code = NULL, description = NULL, ...) {
  Source$new(
    dtconn, primary_keys = primary_keys, binding_keys = binding_keys,
    source_code = source_code, description = description,
    ...
  )
}

#' @rdname source-layer
#' @export
.init_step.tblist <- function(source, ...) {
  source$dtconn
}

#' @rdname source-layer
#' @export
.collect_data.tblist <- function(source, data_object) {
  data_object
}

#' @rdname source-layer
#' @export
.get_stats.tblist <- function(source, data_object) {
  dataset_names <- names(source$dtconn)
  dataset_names |>
    purrr::map(
      ~ list(n_rows = nrow(data_object[[.x]]))
    ) |>
    stats::setNames(dataset_names)
}

# -- S7 method implementations: CbFilterDiscrete x tblist ---------------------

S7::method(cb_filter_data, list(CbFilterDiscrete, tblist_class)) <- function(filter, source, data_object, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  value <- filter@value
  keep_na <- filter@keep_na

  if (keep_na && !identical(value, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(!!sym(variable) %in% !!c(value, NA))
  }
  if (!keep_na && identical(value, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(!is.na(!!sym(variable)))
  }
  if (!keep_na && !identical(value, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(!!sym(variable) %in% !!value)
  }
  attr(data_object[[dataset]], "filtered") <- TRUE
  data_object
}

S7::method(cb_get_filter_stats, list(CbFilterDiscrete, tblist_class)) <- function(filter, source, data_object, name, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  if (missing(name)) {
    name <- c("n_data", "choices", "n_missing")
  }
  stats <- list(
    choices = if ("choices" %in% name) data_object[[dataset]][[variable]] |>
      stats::na.omit() |> table() |> as.list(),
    n_data = if ("n_data" %in% name) data_object[[dataset]][[variable]] |>
      stats::na.omit() |> length(),
    n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] |>
      is.na() |> sum()
  )
  if (length(name) == 1L) stats[[name]] else stats[name]
}

S7::method(cb_plot_filter_data, list(CbFilterDiscrete, tblist_class)) <- function(filter, source, data_object, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  if (nrow(data_object[[dataset]])) {
    data_object[[dataset]][[variable]] |> table() |> prop.table() |> graphics::barplot(...)
  } else {
    graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
  }
}

S7::method(cb_get_filter_data, list(CbFilterDiscrete, tblist_class)) <- function(filter, source, data_object, ...) {
  data_object[[filter@dataset]][[filter@variable]]
}

S7::method(cb_get_filter_defaults, list(CbFilterDiscrete, tblist_class)) <- function(filter, source, data_object, cache_object, ...) {
  list(value = names(cache_object$choices))
}

# -- S7 method implementations: CbFilterDiscreteText x tblist -----------------

S7::method(cb_filter_data, list(CbFilterDiscreteText, tblist_class)) <- function(filter, source, data_object, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  value <- filter@value

  if (!identical(value, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(
        !!sym(variable) %in% !!strsplit(
          sub(" ", "", value, fixed = TRUE),
          split = ",", fixed = TRUE
        )[[1L]]
      )
  }
  attr(data_object[[dataset]], "filtered") <- TRUE
  data_object
}

S7::method(cb_get_filter_stats, list(CbFilterDiscreteText, tblist_class)) <- function(filter, source, data_object, name, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  if (missing(name)) {
    name <- c("n_data", "choices", "n_missing")
  }
  stats <- list(
    choices = if ("choices" %in% name) data_object[[dataset]][[variable]] |>
      collapse::funique() |> paste(collapse = ","),
    n_data = if ("n_data" %in% name) data_object[[dataset]][[variable]] |>
      stats::na.omit() |> collapse::funique() |> length(),
    n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] |>
      is.na() |> sum()
  )
  if (length(name) == 1L) stats[[name]] else stats[name]
}

S7::method(cb_plot_filter_data, list(CbFilterDiscreteText, tblist_class)) <- function(filter, source, data_object, ...) {
  invisible(NULL)
}

S7::method(cb_get_filter_data, list(CbFilterDiscreteText, tblist_class)) <- function(filter, source, data_object, ...) {
  data_object[[filter@dataset]][[filter@variable]]
}

S7::method(cb_get_filter_defaults, list(CbFilterDiscreteText, tblist_class)) <- function(filter, source, data_object, cache_object, ...) {
  list(value = cache_object$choices)
}

# -- Shared helpers for range-based filters -----------------------------------

get_range_frequencies <- function(data_object, dataset, variable, extra_params) {
  step <- 1L
  if (length(stats::na.omit(data_object[[dataset]][[variable]])) == 0L) {
    return(
      data.frame(
        level = character(0L), count = numeric(0L),
        l_bound = numeric(0L), u_bound = numeric(0L),
        stringsAsFactors = FALSE
      )
    )
  }
  min_val <- min(data_object[[dataset]][[variable]], na.rm = TRUE)
  max_val <- max(data_object[[dataset]][[variable]], na.rm = TRUE)
  if (min_val == max_val) {
    return(
      data.frame(
        level = "1", count = length(data_object[[dataset]][[variable]]),
        l_bound = min_val, u_bound = max_val,
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(extra_params$step)) {
    step <- extra_params$step
  }
  breaks <- seq(min_val, max_val, by = step)
  if (rev(breaks)[1L] != max_val) {
    breaks[length(breaks) + 1L] <- max_val
  }
  breaks <- round(breaks, 2L)
  bounds <- breaks

  breaks[1L] <- breaks[1L] - 0.01
  breaks[length(breaks)] <- breaks[length(breaks)] + 0.01

  data_object[[dataset]][, variable, drop = FALSE] |>
    dplyr::filter(!is.na(!!sym(variable))) |>
    dplyr::mutate(
      level = factor(
        findInterval(!!sym(variable), breaks, rightmost.closed = FALSE),
        levels = seq_along(breaks),
        labels = as.character(seq_along(breaks))
      )
    ) |>
    dplyr::group_by(level) |>
    dplyr::summarise(count = dplyr::n()) |>
    tidyr::complete(level, fill = list(count = 0L)) |>
    dplyr::arrange(level) |>
    dplyr::mutate(
      l_bound = bounds,
      u_bound = c(bounds[-1L], bounds[length(bounds)])
    )
}

range_filter_data_impl <- function(filter, data_object) {
  dataset <- filter@dataset
  variable <- filter@variable
  range <- filter@range
  keep_na <- filter@keep_na

  if (keep_na && !identical(range, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(
        (!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L]) |
          is.na(!!sym(variable))
      )
  }
  if (!keep_na && identical(range, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(!is.na(!!sym(variable)))
  }
  if (!keep_na && !identical(range, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L])
  }
  attr(data_object[[dataset]], "filtered") <- TRUE
  data_object
}

range_get_defaults_impl <- function(filter, cache_object) {
  list(
    range = c(
      cache_object$frequencies$l_bound[1L],
      rev(cache_object$frequencies$u_bound)[1L]
    )
  )
}

# -- S7 method implementations: CbFilterRange x tblist ------------------------

S7::method(cb_filter_data, list(CbFilterRange, tblist_class)) <- function(filter, source, data_object, ...) {
  range_filter_data_impl(filter, data_object)
}

S7::method(cb_get_filter_stats, list(CbFilterRange, tblist_class)) <- function(filter, source, data_object, name, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  if (missing(name)) {
    name <- c("n_data", "frequencies", "min", "max", "n_missing")
  }
  extra_params <- filter@extra

  stats <- list(
    frequencies = if ("frequencies" %in% name) {
      get_range_frequencies(data_object, dataset, variable, extra_params)
    },
    min = if ("min" %in% name) min(data_object[[dataset]][[variable]], na.rm = TRUE),
    max = if ("max" %in% name) max(data_object[[dataset]][[variable]], na.rm = TRUE),
    n_data = if ("n_data" %in% name) data_object[[dataset]][[variable]] |>
      stats::na.omit() |> length(),
    n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] |>
      is.na() |> sum()
  )
  if (length(name) == 1L) stats[[name]] else stats[name]
}

S7::method(cb_plot_filter_data, list(CbFilterRange, tblist_class)) <- function(filter, source, data_object, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  if (nrow(data_object[[dataset]])) {
    data_object[[dataset]][[variable]] |> graphics::hist(...)
  } else {
    graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
  }
}

S7::method(cb_get_filter_data, list(CbFilterRange, tblist_class)) <- function(filter, source, data_object, ...) {
  data_object[[filter@dataset]][[filter@variable]]
}

S7::method(cb_get_filter_defaults, list(CbFilterRange, tblist_class)) <- function(filter, source, data_object, cache_object, ...) {
  range_get_defaults_impl(filter, cache_object)
}

# -- Date range frequencies helper --------------------------------------------

get_date_range_frequencies <- function(data_object, dataset, variable, extra_params) {
  step <- "day"
  if (length(stats::na.omit(data_object[[dataset]][[variable]])) == 0L) {
    return(
      data.frame(
        level = character(0L), count = numeric(0L),
        l_bound = numeric(0L), u_bound = numeric(0L),
        stringsAsFactors = FALSE
      )
    )
  }
  min_val <- min(data_object[[dataset]][[variable]], na.rm = TRUE)
  max_val <- max(data_object[[dataset]][[variable]], na.rm = TRUE)
  if (min_val == max_val) {
    return(
      data.frame(
        level = "1", count = length(data_object[[dataset]][[variable]]),
        l_bound = min_val, u_bound = max_val,
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(extra_params$step)) {
    step <- extra_params$step
  }
  breaks <- seq.Date(min_val, max_val, by = step)
  if (rev(breaks)[1L] != max_val) {
    breaks[length(breaks) + 1L] <- max_val
  }

  data_object[[dataset]][, variable, drop = FALSE] |>
    dplyr::filter(!is.na(!!sym(variable))) |>
    dplyr::mutate(
      level = factor(
        findInterval(!!sym(variable), breaks, rightmost.closed = FALSE),
        levels = seq_along(breaks),
        labels = as.character(seq_along(breaks))
      )
    ) |>
    dplyr::group_by(level) |>
    dplyr::summarise(count = dplyr::n()) |>
    tidyr::complete(level, fill = list(count = 0L)) |>
    dplyr::arrange(level) |>
    dplyr::mutate(
      l_bound = breaks,
      u_bound = c(breaks[-1L], breaks[length(breaks)])
    )
}

# -- S7 method implementations: CbFilterDateRange x tblist --------------------

S7::method(cb_filter_data, list(CbFilterDateRange, tblist_class)) <- function(filter, source, data_object, ...) {
  range_filter_data_impl(filter, data_object)
}

S7::method(cb_get_filter_stats, list(CbFilterDateRange, tblist_class)) <- function(filter, source, data_object, name, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  if (missing(name)) {
    name <- c("n_data", "frequencies", "min", "max", "n_missing")
  }
  extra_params <- filter@extra

  stats <- list(
    frequencies = if ("frequencies" %in% name) {
      get_date_range_frequencies(data_object, dataset, variable, extra_params)
    },
    min = if ("min" %in% name) min(data_object[[dataset]][[variable]], na.rm = TRUE),
    max = if ("max" %in% name) max(data_object[[dataset]][[variable]], na.rm = TRUE),
    n_data = if ("n_data" %in% name) data_object[[dataset]][[variable]] |>
      stats::na.omit() |> length(),
    n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] |>
      is.na() |> sum()
  )
  if (length(name) == 1L) stats[[name]] else stats[name]
}

S7::method(cb_plot_filter_data, list(CbFilterDateRange, tblist_class)) <- function(filter, source, data_object, ..., breaks) {
  dataset <- filter@dataset
  variable <- filter@variable
  if (nrow(data_object[[dataset]])) {
    data_object[[dataset]][[variable]] |> graphics::hist(..., breaks = breaks)
  } else {
    graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
  }
}

S7::method(cb_get_filter_data, list(CbFilterDateRange, tblist_class)) <- function(filter, source, data_object, ...) {
  data_object[[filter@dataset]][[filter@variable]]
}

S7::method(cb_get_filter_defaults, list(CbFilterDateRange, tblist_class)) <- function(filter, source, data_object, cache_object, ...) {
  range_get_defaults_impl(filter, cache_object)
}

# -- Datetime helpers ---------------------------------------------------------

col_choices <- function(vec) {
  counts <- table(vec)
  stats::setNames(names(counts), paste(names(counts), glue::glue("({counts})")))
}

group_stats <- function(vec_stats, name) {
  data.frame(val = as.vector(vec_stats), row.names = names(vec_stats)) |>
    stats::setNames(name)
}

calculate_datetime_step <- function(min_date, max_date) {
  steps <- c(
    "mins" = 60L, "hours" = 3600L, "days" = 86400L,
    "weeks" = 604800L, "months" = 2592000L, "years" = 31104000L
  )
  time_span <- as.numeric(max_date) - as.numeric(min_date)
  num_elements <- as.integer(time_span / steps)
  idx <- which(num_elements <= 200L)[1L]
  if (!is.na(idx)) return(steps[idx])
  steps[length(steps)]
}

# -- S7 method implementations: CbFilterDatetimeRange x tblist ----------------

S7::method(cb_filter_data, list(CbFilterDatetimeRange, tblist_class)) <- function(filter, source, data_object, ...) {
  range_filter_data_impl(filter, data_object)
}

S7::method(cb_get_filter_stats, list(CbFilterDatetimeRange, tblist_class)) <- function(filter, source, data_object, name, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  if (missing(name)) {
    name <- c("n_data", "frequencies", "min", "max", "n_missing")
  }
  extra_params <- filter@extra

  data_object[[dataset]][[variable]] <- as.numeric(data_object[[dataset]][[variable]])

  if (is.null(extra_params$step) && !identical(length(data_object[[dataset]][[variable]]), 0L)) {
    min_val <- min(data_object[[dataset]][[variable]], na.rm = TRUE)
    max_val <- max(data_object[[dataset]][[variable]], na.rm = TRUE)
    extra_params$step <- calculate_datetime_step(min_val, max_val) |> unname()
  }

  stats <- list(
    frequencies = if ("frequencies" %in% name) {
      get_range_frequencies(data_object, dataset, variable, extra_params)
    },
    min = if ("min" %in% name) min(data_object[[dataset]][[variable]], na.rm = TRUE),
    max = if ("max" %in% name) max(data_object[[dataset]][[variable]], na.rm = TRUE),
    n_data = if ("n_data" %in% name) data_object[[dataset]][[variable]] |>
      stats::na.omit() |> length(),
    n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] |>
      is.na() |> sum()
  )
  if (length(name) == 1L) stats[[name]] else stats[name]
}

S7::method(cb_plot_filter_data, list(CbFilterDatetimeRange, tblist_class)) <- function(filter, source, data_object, ..., breaks = NULL) {
  dataset <- filter@dataset
  variable <- filter@variable
  if (nrow(data_object[[dataset]])) {
    if (is.null(breaks)) {
      breaks <- calculate_datetime_step(
        min(data_object[[dataset]][[variable]], na.rm = TRUE),
        max(data_object[[dataset]][[variable]], na.rm = TRUE)
      ) |> names()
    }
    data_object[[dataset]][[variable]] |> graphics::hist(..., breaks = breaks)
  } else {
    graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
  }
}

S7::method(cb_get_filter_data, list(CbFilterDatetimeRange, tblist_class)) <- function(filter, source, data_object, ...) {
  data_object[[filter@dataset]][[filter@variable]]
}

S7::method(cb_get_filter_defaults, list(CbFilterDatetimeRange, tblist_class)) <- function(filter, source, data_object, cache_object, ...) {
  range_get_defaults_impl(filter, cache_object)
}

# -- S7 method implementations: CbFilterMultiDiscrete x tblist ----------------

S7::method(cb_filter_data, list(CbFilterMultiDiscrete, tblist_class)) <- function(filter, source, data_object, ...) {
  dataset <- filter@dataset
  values <- filter@values
  keep_na <- filter@keep_na

  col_in_val <- function(vec, value, keep_na) {
    if (identical(value, NA)) {
      val_mask <- rep(TRUE, length(vec))
    } else if (is.null(value)) {
      val_mask <- rep(FALSE, length(vec))
    } else {
      val_mask <- vec %in% value
    }
    if (keep_na) is.na(vec) | val_mask else !is.na(vec) & val_mask
  }

  data_object[[dataset]] <- data_object[[dataset]] |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(names(values)),
        ~ col_in_val(.x, values[[deparse(substitute(.x))]], !!keep_na)
      )
    )
  attr(data_object[[dataset]], "filtered") <- TRUE
  data_object
}

S7::method(cb_get_filter_stats, list(CbFilterMultiDiscrete, tblist_class)) <- function(filter, source, data_object, name, ...) {
  dataset <- filter@dataset
  variables <- unlist(filter@variables)
  if (missing(name)) {
    name <- c("n_data", "choices", "n_missing")
  }
  stats <- list(
    choices = if ("choices" %in% name) data_object[[dataset]][variables] |>
      purrr::map(~ as.list(table(.))),
    n_data = if ("n_data" %in% name) nrow(data_object[[dataset]][variables]),
    n_missing = if ("n_missing" %in% name) data_object[[dataset]][variables] |>
      is.na() |> colSums() |> as.list()
  )
  if (length(name) == 1L) stats[[name]] else stats[name]
}

S7::method(cb_plot_filter_data, list(CbFilterMultiDiscrete, tblist_class)) <- function(filter, source, data_object, ...) {
  dataset <- filter@dataset
  variables <- unlist(filter@variables)
  if (nrow(data_object[[dataset]])) {
    data_object[[dataset]][variables] |>
      purrr::map(table) |>
      purrr::imap_dfc(group_stats) |>
      as.matrix() |>
      graphics::barplot(...)
  } else {
    graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
  }
}

S7::method(cb_get_filter_data, list(CbFilterMultiDiscrete, tblist_class)) <- function(filter, source, data_object, ...) {
  data_object[[filter@dataset]][, filter@variables]
}

S7::method(cb_get_filter_defaults, list(CbFilterMultiDiscrete, tblist_class)) <- function(filter, source, data_object, cache_object, ...) {
  list(values = names(cache_object$choices))
}

# -- S7 method implementations: CbFilterQuery x tblist ------------------------

S7::method(cb_filter_data, list(CbFilterQuery, tblist_class)) <- function(filter, source, data_object, ...) {
  dataset <- filter@dataset
  value <- filter@value
  keep_na <- filter@keep_na

  if (keep_na && !identical(value, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(!!queryBuilder::queryToExpr(value, keep_na = keep_na))
  }
  if (!keep_na && !identical(value, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(!!queryBuilder::queryToExpr(value))
  }
  attr(data_object[[dataset]], "filtered") <- TRUE
  data_object
}

S7::method(cb_get_filter_stats, list(CbFilterQuery, tblist_class)) <- function(filter, source, data_object, name, ...) {
  dataset <- filter@dataset
  variables <- unlist(filter@variables)
  if (missing(name)) {
    name <- c("n_data", "specs", "n_missing")
  }
  stat_from_column <- base::get("stat_from_column", envir = asNamespace("queryBuilder"), inherits = FALSE)
  stats <- list(
    specs = if ("specs" %in% name) data_object[[dataset]][variables] |>
      purrr::imap(stat_from_column),
    n_data = if ("n_data" %in% name) nrow(data_object[[dataset]][variables]),
    n_missing = if ("n_missing" %in% name) data_object[[dataset]][variables] |>
      is.na() |> colSums() |> as.list()
  )
  if (length(name) == 1L) stats[[name]] else stats[name]
}

S7::method(cb_plot_filter_data, list(CbFilterQuery, tblist_class)) <- function(filter, source, data_object, ...) {
  dataset <- filter@dataset
  variables <- filter@variables
  if (nrow(data_object[[dataset]])) {
    data_object[[dataset]][variables] |>
      purrr::map(table) |>
      purrr::imap_dfc(group_stats) |>
      as.matrix() |>
      graphics::barplot(...)
  } else {
    graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
  }
}

S7::method(cb_get_filter_data, list(CbFilterQuery, tblist_class)) <- function(filter, source, data_object, ...) {
  data_object[[filter@dataset]][, filter@variables, drop = FALSE]
}

S7::method(cb_get_filter_defaults, list(CbFilterQuery, tblist_class)) <- function(filter, source, data_object, cache_object, ...) {
  list(value = names(cache_object$choices))
}

# -- S7 method implementations: cb_filter_to_expr x tblist --------------------

S7::method(cb_filter_to_expr, list(CbFilterDiscrete, tblist_class)) <- function(filter, source, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  value <- filter@value
  keep_na <- filter@keep_na

  if (keep_na && !identical(value, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!!sym(variable) %in% !!c(value, NA))
    })
  } else if (!keep_na && identical(value, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!is.na(!!sym(variable)))
    })
  } else if (!keep_na && !identical(value, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!!sym(variable) %in% !!value)
    })
  } else {
    NULL
  }
}

S7::method(cb_filter_to_expr, list(CbFilterDiscreteText, tblist_class)) <- function(filter, source, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  value <- filter@value

  if (!identical(value, NA)) {
    split_values <- strsplit(sub(" ", "", value, fixed = TRUE), split = ",", fixed = TRUE)[[1L]]
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!!sym(variable) %in% !!split_values)
    })
  } else {
    NULL
  }
}

S7::method(cb_filter_to_expr, list(CbFilterRange, tblist_class)) <- function(filter, source, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  range <- filter@range
  keep_na <- filter@keep_na

  if (keep_na && !identical(range, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(
          (!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L]) |
            is.na(!!sym(variable))
        )
    })
  } else if (!keep_na && identical(range, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!is.na(!!sym(variable)))
    })
  } else if (!keep_na && !identical(range, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L])
    })
  } else {
    NULL
  }
}

S7::method(cb_filter_to_expr, list(CbFilterDateRange, tblist_class)) <- function(filter, source, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  range <- filter@range
  keep_na <- filter@keep_na

  if (keep_na && !identical(range, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(
          (!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L]) |
            is.na(!!sym(variable))
        )
    })
  } else if (!keep_na && identical(range, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!is.na(!!sym(variable)))
    })
  } else if (!keep_na && !identical(range, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L])
    })
  } else {
    NULL
  }
}

S7::method(cb_filter_to_expr, list(CbFilterDatetimeRange, tblist_class)) <- function(filter, source, ...) {
  dataset <- filter@dataset
  variable <- filter@variable
  range <- filter@range
  keep_na <- filter@keep_na

  if (keep_na && !identical(range, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(
          (!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L]) |
            is.na(!!sym(variable))
        )
    })
  } else if (!keep_na && identical(range, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!is.na(!!sym(variable)))
    })
  } else if (!keep_na && !identical(range, NA)) {
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L])
    })
  } else {
    NULL
  }
}

S7::method(cb_filter_to_expr, list(CbFilterMultiDiscrete, tblist_class)) <- function(filter, source, ...) {
  dataset <- filter@dataset
  values <- filter@values
  keep_na <- filter@keep_na

  if (all(purrr::map_lgl(values, ~ identical(.x, NA)))) {
    if (!keep_na) {
      vars <- names(values)
      rlang::expr({
        data_object[[!!dataset]] <- data_object[[!!dataset]] |>
          dplyr::filter(dplyr::if_all(dplyr::all_of(!!vars), ~ !is.na(.x)))
      })
    } else {
      NULL
    }
  } else {
    filter_exprs <- purrr::imap(values, function(val, var) {
      if (identical(val, NA)) return(NULL)
      if (keep_na) {
        rlang::expr(!!sym(var) %in% !!c(val, NA))
      } else {
        rlang::expr(!!sym(var) %in% !!val)
      }
    }) |> purrr::compact()

    if (length(filter_exprs) == 0L) return(NULL)

    combined <- Reduce(function(a, b) rlang::expr(!!a & !!b), filter_exprs)
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!!combined)
    })
  }
}

S7::method(cb_filter_to_expr, list(CbFilterQuery, tblist_class)) <- function(filter, source, ...) {
  dataset <- filter@dataset
  value <- filter@value
  keep_na <- filter@keep_na

  if (!identical(value, NA)) {
    query_expr <- queryBuilder::queryToExpr(value, keep_na = keep_na)
    rlang::expr({
      data_object[[!!dataset]] <- data_object[[!!dataset]] |>
        dplyr::filter(!!query_expr)
    })
  } else {
    NULL
  }
}

# -- Source layer methods (non-filter) ----------------------------------------

#' @export
.run_binding.tblist <- function(source, binding_key, data_object_pre, data_object_post, ...) {
  binding_dataset <- binding_key$update$dataset
  dependent_datasets <- names(binding_key$data_keys)
  active_datasets <- data_object_post |>
    purrr::keep(~ attr(., "filtered")) |>
    names()

  if (!any(dependent_datasets %in% active_datasets)) {
    return(data_object_post)
  }

  key_values <- NULL
  common_key_names <- paste0("key_", seq_along(binding_key$data_keys[[1L]]$key))
  for (dependent_dataset in dependent_datasets) {
    key_names <- binding_key$data_keys[[dependent_dataset]]$key
    tmp_key_values <- collapse::funique(data_object_post[[dependent_dataset]][, key_names, drop = FALSE]) |>
      stats::setNames(common_key_names)
    if (is.null(key_values)) {
      key_values <- tmp_key_values
    } else {
      key_values <- dplyr::inner_join(key_values, tmp_key_values, by = common_key_names)
    }
  }

  df <- switch(
    as.character(binding_key$post),
    "FALSE" = data_object_pre[[binding_dataset]],
    "TRUE" = data_object_post[[binding_dataset]]
  )

  data_object_post[[binding_dataset]] <- tryCatch({
    collapse::join(
      df, key_values,
      on = stats::setNames(common_key_names, binding_key$update$key),
      how = "inner",
      verbose = getOption("cb_verbose", default = FALSE)
    )
  }, error = function(e) {
    dplyr::inner_join(
      df, key_values,
      by = stats::setNames(common_key_names, binding_key$update$key)
    )
  })

  if (binding_key$activate) {
    attr(data_object_post[[binding_dataset]], "filtered") <- TRUE
  }

  data_object_post
}

#' @export
.get_attrition_label.tblist <- function(source, step_id, step_filters, dataset, ...) {
  if (missing(dataset)) {
    stop(glue::glue(
      "Argument {sQuote('dataset')} is required to print attrition plot for 'tblist' Source."
    ))
  }
  pkey <- source$primary_keys
  binding_keys <- source$binding_keys
  if (step_id == "0") {
    if (is.null(pkey)) {
      return(dataset)
    } else {
      dataset_pkey <- .get_item(pkey, "dataset", dataset)[1L][[1L]]$key
      if (is.null(dataset_pkey)) return(dataset)
      return(glue::glue("{dataset}\n primary key: {paste(dataset_pkey, collapse = ', ')}"))
    }
  }
  filters_section <- step_filters |>
    purrr::keep(~ .$dataset == dataset) |>
    purrr::map(~ get_attrition_filter_label(.$name, .$value_name, .$value)) |>
    paste(collapse = "\n")
  bind_keys_section <- ""
  if (!is.null(binding_keys)) {
    dependent_datasets <- .get_item(
      binding_keys, attribute = "update", value = dataset,
      operator = function(value, target) {
        value == target$dataset
      }
    ) |>
      purrr::map(~ names(.[["data_keys"]])) |>
      unlist() |>
      collapse::funique()
    if (length(dependent_datasets) > 0L) {
      bind_keys_section <- glue::glue(
        "\nData linked with external datasets: {paste(dependent_datasets, collapse = ', ')}",
        .trim = FALSE
      )
    }
  }
  gsub(
    "\n$", "",
    glue::glue("Step: {step_id}\n{filters_section}{bind_keys_section}")
  )
}

#' @export
.get_attrition_count.tblist <- function(source, data_stats, dataset, ...) {
  if (missing(dataset)) {
    stop(glue::glue(
      "Argument {sQuote('dataset')} is required to print attrition plot for 'tblist' Source."
    ))
  }
  data_stats |>
    purrr::map_int(~ .[[dataset]][["n_rows"]])
}

#' @export
.pre_filtering.tblist <- function(source, data_object, step_id) {
  for (dataset in names(data_object)) {
    attr(data_object[[dataset]], "filtered") <- FALSE
  }
  data_object
}

#' @export
.repro_code_tweak.tblist <- function(source, code_data) {
  pipe_all_filters(code_data)
}

#' @export
shape.tblist <- function(source, field, subfield, ...) {
  description_obj <- source$description
  if (missing(subfield)) {
    subfield <- "dataset_"
  }
  if (!missing(field)) {
    field_val <- description_obj[[field]]
    if (is.character(field_val)) {
      return(field_val)
    }
    return(field_val[[subfield]]$text)
  }
  purrr::imap_dfr(
    description_obj,
    function(fields, dataset_name) {
      purrr::imap_dfr(
        fields,
        function(field, field_name) {
          stats_container <- source$meta_stats$filters
          if (field_name == "dataset_") {
            field_name <- NA
          }
          stats <- source$meta_stats$filters[[field_name]]
          stats <- stats[names(stats) %in% c("min", "max", "choices")]
          stats$type <- "range"
          if ("choices" %in% names(stats)) {
            stats$choices <- names(stats$choices)
            stats$type <- "discrete"
          }
          tibble::tibble(
            dataset = dataset_name, filter = field_name, description = field,
            stats = list(stats)
          )
        }
      )
    }
  )
}

# -- Autofilter rules ---------------------------------------------------------

rule_character <- function(column, name, dataset_name) {
  type <- "discrete"
  gui_input <- NULL
  n_unique <- length(unique(column))
  if (n_unique == length(column)) {
    type <- "discrete_text"
  } else if (length(unique(column)) > 3) {
    gui_input <- "vs"
  }
  drop_nulls(
    list(
      type = type, id = name, name = name, variable = name,
      dataset = dataset_name, value = NA, keep_na = TRUE,
      gui_input = gui_input
    )
  )
}

rule_factor <- function(column, name, dataset_name) {
  type <- "discrete"
  gui_input <- NULL
  n_levels <- length(levels(column))
  if (n_levels == length(column)) {
    type <- "discrete_text"
  } else if (length(unique(column)) > 3) {
    gui_input <- "vs"
  }
  drop_nulls(
    list(
      type = type, id = name, name = name, variable = name,
      dataset = dataset_name, value = NA, keep_na = TRUE,
      gui_input = gui_input
    )
  )
}

rule_numeric <- function(column, name, dataset_name) {
  list(
    type = "range", id = name, name = name, variable = name,
    dataset = dataset_name, range = NA, keep_na = TRUE
  )
}
rule_integer <- rule_numeric

rule_Date <- function(column, name, dataset_name) {
  list(
    type = "date_range", id = name, name = name, variable = name,
    dataset = dataset_name, range = NA, keep_na = TRUE
  )
}

rule_POSIXct <- function(column, name, dataset_name) {
  list(
    type = "datetime_range", id = name, name = name, variable = name,
    dataset = dataset_name, range = NA, keep_na = TRUE
  )
}

filter_rule <- function(column, name, dataset_name) {
  rule_method <- paste0("rule_", class(column)[[1]])
  do.call(
    rule_method,
    list(column = column, name = name, dataset_name = dataset_name)
  )
}

filter_rules <- function(dataset, dataset_name) {
  dataset |>
    purrr::imap(~ filter_rule(.x, .y, dataset_name = dataset_name))
}

#' @rdname autofilter
#' @export
autofilter.tblist <- function(source, attach_as = c("step", "meta"), ...) {
  attach_as <- rlang::arg_match(attach_as)
  step_rule <- source$dtconn |>
    purrr::imap(~ filter_rules(.x, .y)) |>
    unlist(recursive = FALSE) |>
    purrr::discard(~ is.null(.x)) |>
    purrr::map(~ do.call(cohortBuilder::filter, .)) |>
    unname()

  if (identical(attach_as, "meta")) {
    source$available_filters <- step_rule
  } else {
    source |>
      cohortBuilder::add_step(do.call(cohortBuilder::step, step_rule))
  }

  return(source)
}
