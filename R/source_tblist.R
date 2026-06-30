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
                              source_code = NULL, description = NULL, available_filters = NULL,
                              compute_meta_stats = getOption("cb.source_filters_meta_stats", TRUE), ...) {
  Source$new(
    dtconn, primary_keys = primary_keys, binding_keys = binding_keys,
    source_code = source_code, description = description,
    available_filters = available_filters, compute_meta_stats = compute_meta_stats,
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
  value <- cb_intersect_domain(filter)
  keep_na <- filter@keep_na

  # Coerce a factor value to character. `c(factor, NA)` would otherwise collapse
  # the factor to its integer codes, making the `%in%` match against the column
  # fail silently and drop every row.
  if (is.factor(value)) {
    value <- as.character(value)
  }

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
  value <- cb_intersect_domain(filter)

  if (!identical(value, NA)) {
    data_object[[dataset]] <- data_object[[dataset]] |>
      dplyr::filter(
        !!sym(variable) %in% !!split_discrete_text(value)
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

#' Bin a numeric variable into frequency counts
#'
#' Cuts a numeric column into intervals (step from `extra_params$step`, default
#' `1`) and counts observations per bin, returning lower/upper bounds.
#'
#' @param data_object Named list of tibbles.
#' @param dataset Dataset name.
#' @param variable Numeric column name.
#' @param extra_params Filter extras; may carry `step`.
#' @return A data frame with `level`, `count`, `l_bound`, `u_bound`.
#' @noRd
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

#' Apply a range/date/datetime filter to a tblist data object
#'
#' Shared implementation for range-type filters: keeps rows within the
#' (domain-intersected) range, honoring `keep_na`.
#'
#' @param filter S7 range-type filter.
#' @param data_object Named list of tibbles.
#' @return The filtered `data_object` with a `filtered` attribute set.
#' @noRd
range_filter_data_impl <- function(filter, data_object) {
  dataset <- filter@dataset
  variable <- filter@variable
  range <- cb_intersect_domain(filter)
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

#' Default range from cached frequency bounds
#'
#' @param filter S7 range-type filter (unused).
#' @param cache_object Cached stats with `frequencies` bounds.
#' @return A list with `range = c(min, max)`.
#' @noRd
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

#' Bin a date variable into frequency counts
#'
#' Like [get_range_frequencies()] but for `Date` columns, using `seq.Date()`
#' with step from `extra_params$step` (default `"day"`).
#'
#' @param data_object Named list of tibbles.
#' @param dataset Dataset name.
#' @param variable Date column name.
#' @param extra_params Filter extras; may carry `step`.
#' @return A data frame with `level`, `count`, `l_bound`, `u_bound`.
#' @noRd
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

#' Build named choice labels with counts for a vector
#'
#' @param vec A vector of observed values.
#' @return A named character vector: values named `"<value> (<count>)"`.
#' @noRd
col_choices <- function(vec) {
  counts <- table(vec)
  stats::setNames(names(counts), paste(names(counts), glue::glue("({counts})")))
}

#' Convert a named stats vector to a single-column data frame
#'
#' @param vec_stats A named numeric vector.
#' @param name Name to give the resulting column.
#' @return A one-column data frame with row names from `vec_stats`.
#' @noRd
group_stats <- function(vec_stats, name) {
  data.frame(val = as.vector(vec_stats), row.names = names(vec_stats)) |>
    stats::setNames(name)
}

#' Pick a datetime binning step keeping bin count manageable
#'
#' Chooses the smallest unit (mins..years) yielding at most ~200 bins across the
#' time span.
#'
#' @param min_date,max_date Range endpoints (datetimes).
#' @return A named integer (seconds per chosen unit).
#' @noRd
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
  values <- cb_intersect_domain(filter)
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
  value <- cb_intersect_domain(filter)
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
  value <- cb_intersect_domain(filter)

  if (!identical(value, NA)) {
    split_values <- split_discrete_text(value)
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
  range <- cb_intersect_domain(filter)
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
  range <- cb_intersect_domain(filter)
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
  range <- cb_intersect_domain(filter)
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
  values <- cb_intersect_domain(filter)
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

#' Extract plain description text from a describe object or string
#'
#' Pulls the `text` from a `describe()` object (a list with a `text` field), a
#' bare character string, or `NULL`. Returns `NULL` when no usable text exists.
#'
#' @param x A `describe()` object, character string, or `NULL`.
#' @return A description string, or `NULL`.
#' @noRd
description_text <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x)) x <- x$text
  if (is.null(x) || !is.character(x) || !nzchar(x)) return(NULL)
  x
}

#' Build a filter's variable list with descriptions
#'
#' One entry per variable the filter operates on, each with its name and the
#' description looked up from the source description (`NA` when undescribed).
#'
#' @param filter S7 filter object.
#' @param source Source object.
#' @return A list of `list(name, description)` entries.
#' @noRd
build_filter_variables <- function(filter, source) {
  vars <- filter_variables(filter)
  dataset_desc <- source$description[[filter@dataset]]
  purrr::map(vars, function(v) {
    list(name = v, description = description_text(dataset_desc[[v]]) %||% NA_character_)
  })
}

#' Build a human-readable filter description
#'
#' Combines the filter name and the filter-level description (when set).
#' Variable descriptions are exposed separately in the `variables` field.
#'
#' @param filter S7 filter object.
#' @return A single description string.
#' @noRd
build_filter_description <- function(filter) {
  parts <- c(filter@name, description_text(filter@description))
  paste(parts, collapse = ". ")
}

#' Resolve a filter's domain for `shape()`
#'
#' Prefers the declared `@domain`, otherwise derives it from stored meta stats
#' (e.g. observed choices / min-max).
#'
#' @param filter S7 filter object.
#' @param source Source object.
#' @return The filter domain, or `NULL`.
#' @noRd
filter_shape_domain <- function(filter, source) {
  domain <- filter@domain
  if (!is.null(domain)) return(domain)
  stats <- source$meta_stats$filters[[filter@id]]
  if (is.null(stats)) return(NULL)
  cb_domain_from_stats(filter, stats)
}

#' @rdname shape
#' @export
shape.tblist <- function(source, field, subfield, ...) {
  description_obj <- source$description

  # Description lookup mode: `shape(source, field[, subfield])` returns the
  # description text for a dataset (default) or one of its fields. Used by the
  # Cohort `show_help()` method.
  if (!missing(field)) {
    if (missing(subfield)) {
      subfield <- "dataset_"
    }
    field_val <- description_obj[[field]]
    if (is.character(field_val)) {
      return(field_val)
    }
    return(field_val[[subfield]]$text)
  }

  # Metadata mode: return a structured list describing datasets and the
  # available filters, intended for LLM tools and programmatic inspection.
  dataset_names <- names(source$dtconn)
  datasets <- purrr::map(
    rlang::set_names(dataset_names),
    ~ description_text(description_obj[[.x]][["dataset_"]]) %||% NA_character_
  )

  filters <- source$available_filters %||% list()
  filters_shape <- filters |>
    purrr::map(function(filter) {
      list(
        dataset = filter@dataset,
        type = filter@type,
        description = build_filter_description(filter),
        variables = build_filter_variables(filter, source),
        domain = filter_shape_domain(filter, source)
      )
    }) |>
    rlang::set_names(purrr::map_chr(filters, ~ .x@id))

  list(
    datasets = datasets,
    filters = filters_shape
  )
}

#' @rdname dot-propagate_domains
#' @export
.propagate_domains.tblist <- function(source, data_object, step_id, cohort, mode, ...) {
  if (is.null(cohort)) return(invisible(NULL))
  target_id <- as.character(step_id)
  parent_id <- prev_step(target_id)
  # No parent (step "1" or below) -> nothing to narrow from.
  if (as.integer(target_id) <= 1L) return(invisible(NULL))

  target_step_obj <- cohort$get_step(target_id)
  if (is.null(target_step_obj)) return(invisible(NULL))

  parent_step_obj <- cohort$get_step(parent_id)
  if (is.null(parent_step_obj)) return(invisible(NULL))
  parent_filters <- parent_step_obj$filters

  for (filter_id in names(target_step_obj$filters)) {
    filter_obj <- target_step_obj$filters[[filter_id]]
    if (is.null(filter_obj@domain)) next

    new_domain <- switch(mode,
      filter = domain_from_filter(filter_obj, cohort, parent_id, source),
      stats = {
        matched <- find_matching_filter(filter_obj, parent_filters)
        if (is.null(matched)) NULL
        else cb_domain_from_stats(
          filter_obj,
          cohort$get_stats(parent_id, matched@id, state = "post", .recalc_when_missing = FALSE)
        )
      },
      data = cb_domain_from_data(filter_obj, source, data_object)
    )

    if (!is.null(new_domain)) {
      cohort$set_domain(target_id, filter_id, new_domain)
    }
  }

  invisible(NULL)
}

# -- Domain propagation helpers ------------------------------------------------

#' Find the filter matching a target filter by id
#'
#' Filter ids are deterministic (derived from dataset + variable when not set
#' explicitly), so the same logical filter keeps the same id across steps and
#' can be matched directly.
#'
#' @param target_filter The filter to match.
#' @param filters Named list of filters to search.
#' @return The matching filter, or `NULL`.
#' @noRd
find_matching_filter <- function(target_filter, filters) {
  filters[[target_filter@id]]
}

#' Compute a `"filter"`-mode propagated domain
#'
#' Narrows the target filter's domain from the same logical filter found in
#' *previous* steps. Searches from the parent step back toward step 1 and
#' intersects the effective domains found, so the target is restricted by the
#' filter wherever it was set upstream. When no previous step contains the
#' filter, falls back to the definition declared in `source$available_filters`
#' (its `@domain`). Purely value-/definition-based: no data access.
#'
#' @param target_filter The filter whose domain is being narrowed.
#' @param cohort The Cohort object.
#' @param parent_id Id of the parent (previous) step.
#' @param source Source object (for the fallback definition).
#' @return The propagated domain, or `NULL`.
#' @noRd
domain_from_filter <- function(target_filter, cohort, parent_id, source) {
  found_any <- FALSE
  effective <- NULL
  for (sid in steps_range("1", parent_id)) {
    upstream_filters <- cohort$get_step(sid)$filters
    match <- find_matching_filter(target_filter, upstream_filters)
    if (is.null(match)) next
    candidate <- cb_intersect_domain(match)
    if (identical(candidate, NA) || is.null(candidate)) next
    found_any <- TRUE
    effective <- if (is.null(effective)) {
      candidate
    } else {
      cb_intersect_domain_values(target_filter, effective, candidate)
    }
  }

  if (found_any) {
    return(effective)
  }

  # Fallback: declared domain from the source's available_filters definition.
  available <- source$available_filters
  if (!is.null(available)) {
    declared <- available[[target_filter@id]]
    if (!is.null(declared) && !is.null(declared@domain)) {
      return(declared@domain)
    }
  }

  NULL
}

# -- cb_domain_from_data: tblist methods ---------------------------------------

#' Data-derived domain for a discrete filter (tblist)
#'
#' Returns the distinct non-`NA` observed values as a *character* domain (not a
#' factor): a factor domain breaks discrete filtering downstream, and this keeps
#' the result consistent with `stats`-mode propagation.
#'
#' @param filter S7 discrete filter.
#' @param source Source object.
#' @param data_object Named list of tibbles.
#' @param ... Unused.
#' @return A character vector of observed values.
#' @noRd
domain_from_data_discrete_impl <- function(filter, source, data_object, ...) {
  column <- data_object[[filter@dataset]][[filter@variable]]
  # Return a character domain (not a factor). A factor domain breaks discrete
  # filtering downstream: `c(factor_value, NA)` coerces the factor to integer
  # codes, so `column %in% c(value, NA)` matches nothing and the step yields
  # zero rows. `stats`-mode propagation already yields character domains, so
  # this also keeps the two modes consistent.
  as.character(collapse::funique(stats::na.omit(column)))
}

S7::method(cb_domain_from_data, list(CbFilterDiscrete, tblist_class)) <- domain_from_data_discrete_impl

# discrete_text represents its value/choices/domain as a single comma-separated
# string, so its data-derived domain must be that string form too (not the
# character vector used by the plain discrete filter), to round-trip through
# cb_intersect_domain() and match the `choices` stat.
S7::method(cb_domain_from_data, list(CbFilterDiscreteText, tblist_class)) <- function(filter, source, data_object, ...) {
  values <- domain_from_data_discrete_impl(filter, source, data_object, ...)
  join_discrete_text(values)
}

#' Data-derived domain for a range-type filter (tblist)
#'
#' @param filter S7 range/date/datetime filter.
#' @param source Source object.
#' @param data_object Named list of tibbles.
#' @param ... Unused.
#' @return A length-2 `c(min, max)` range, or `NULL` when no data.
#' @noRd
domain_from_data_range_impl <- function(filter, source, data_object, ...) {
  column <- stats::na.omit(data_object[[filter@dataset]][[filter@variable]])
  if (length(column) == 0L) return(NULL)
  c(min(column), max(column))
}

S7::method(cb_domain_from_data, list(CbFilterRange, tblist_class)) <- domain_from_data_range_impl
S7::method(cb_domain_from_data, list(CbFilterDateRange, tblist_class)) <- domain_from_data_range_impl
S7::method(cb_domain_from_data, list(CbFilterDatetimeRange, tblist_class)) <- domain_from_data_range_impl

S7::method(cb_domain_from_data, list(CbFilterMultiDiscrete, tblist_class)) <- function(filter, source, data_object, ...) {
  variables <- names(filter@values)
  dataset <- data_object[[filter@dataset]]
  purrr::map(rlang::set_names(variables), ~ collapse::funique(stats::na.omit(dataset[[.x]])))
}

# -- Autofilter rules ---------------------------------------------------------

#' Autofilter rule for character columns
#'
#' Produces a `discrete` filter spec (or `discrete_text` when every value is
#' unique); uses the `"vs"` GUI input above 3 distinct values.
#'
#' @param column The data column.
#' @param name Variable/filter name.
#' @param dataset_name Owning dataset name.
#' @param field_description Optional field description (may carry `domain`).
#' @return A named list of `filter()` arguments.
#' @noRd
rule_character <- function(column, name, dataset_name, field_description = NULL) {
  type <- "discrete"
  gui_input <- NULL
  n_unique <- length(unique(column))
  if (n_unique == length(column)) {
    type <- "discrete_text"
  } else if (length(unique(column)) > 3) {
    gui_input <- "vs"
  }
  domain <- field_description$domain %||% collapse::funique(column)
  drop_nulls(
    list(
      type = type, id = name, name = name, variable = name,
      dataset = dataset_name, value = NA, keep_na = TRUE,
      domain = domain, gui_input = gui_input
    )
  )
}

#' Autofilter rule for factor columns
#'
#' Produces a `discrete` filter spec (or `discrete_text` when every value is
#' unique) using the factor's levels as domain.
#'
#' @param column The data column.
#' @param name Variable/filter name.
#' @param dataset_name Owning dataset name.
#' @param field_description Optional field description (may carry `domain`).
#' @return A named list of `filter()` arguments.
#' @noRd
rule_factor <- function(column, name, dataset_name, field_description = NULL) {
  type <- "discrete"
  gui_input <- NULL
  n_levels <- length(levels(column))
  if (n_levels == length(column)) {
    type <- "discrete_text"
  } else if (length(unique(column)) > 3) {
    gui_input <- "vs"
  }
  domain <- field_description$domain %||% levels(column)
  drop_nulls(
    list(
      type = type, id = name, name = name, variable = name,
      dataset = dataset_name, value = NA, keep_na = TRUE,
      domain = domain, gui_input = gui_input
    )
  )
}

#' Autofilter rule for numeric columns
#'
#' Produces a `range` filter spec with domain `c(min, max)`.
#'
#' @param column The data column.
#' @param name Variable/filter name.
#' @param dataset_name Owning dataset name.
#' @param field_description Optional field description (may carry `domain`).
#' @return A named list of `filter()` arguments.
#' @noRd
rule_numeric <- function(column, name, dataset_name, field_description = NULL) {
  domain <- field_description$domain %||% c(min(column, na.rm = TRUE), max(column, na.rm = TRUE))
  list(
    type = "range", id = name, name = name, variable = name,
    dataset = dataset_name, range = NA, keep_na = TRUE,
    domain = domain
  )
}

#' Autofilter rule for integer columns (alias of [rule_numeric()])
#' @noRd
rule_integer <- rule_numeric

#' Autofilter rule for Date columns
#'
#' Produces a `date_range` filter spec with domain `c(min, max)`.
#'
#' @param column The data column.
#' @param name Variable/filter name.
#' @param dataset_name Owning dataset name.
#' @param field_description Optional field description (may carry `domain`).
#' @return A named list of `filter()` arguments.
#' @noRd
rule_Date <- function(column, name, dataset_name, field_description = NULL) {
  domain <- field_description$domain %||% c(min(column, na.rm = TRUE), max(column, na.rm = TRUE))
  list(
    type = "date_range", id = name, name = name, variable = name,
    dataset = dataset_name, range = NA, keep_na = TRUE,
    domain = domain
  )
}

#' Autofilter rule for POSIXct columns
#'
#' Produces a `datetime_range` filter spec with domain `c(min, max)`.
#'
#' @param column The data column.
#' @param name Variable/filter name.
#' @param dataset_name Owning dataset name.
#' @param field_description Optional field description (may carry `domain`).
#' @return A named list of `filter()` arguments.
#' @noRd
rule_POSIXct <- function(column, name, dataset_name, field_description = NULL) {
  domain <- field_description$domain %||% c(min(column, na.rm = TRUE), max(column, na.rm = TRUE))
  list(
    type = "datetime_range", id = name, name = name, variable = name,
    dataset = dataset_name, range = NA, keep_na = TRUE,
    domain = domain
  )
}

#' Dispatch to the autofilter rule for a column's class
#'
#' Calls `rule_<class>()` based on the column's first class.
#'
#' @param column The data column.
#' @param name Variable/filter name.
#' @param dataset_name Owning dataset name.
#' @param field_description Optional field description.
#' @return A named list of `filter()` arguments.
#' @noRd
filter_rule <- function(column, name, dataset_name, field_description = NULL) {
  rule_method <- paste0("rule_", class(column)[[1]])
  do.call(
    rule_method,
    list(column = column, name = name, dataset_name = dataset_name,
         field_description = field_description)
  )
}

#' Build autofilter rules for every column of a dataset
#'
#' @param dataset A single dataset (tibble/data frame).
#' @param dataset_name The dataset's name.
#' @param description Optional per-field descriptions.
#' @return A list of `filter()` argument lists, one per column.
#' @noRd
filter_rules <- function(dataset, dataset_name, description = NULL) {
  dataset |>
    purrr::imap(~ filter_rule(.x, .y, dataset_name = dataset_name,
                              field_description = description[[.y]]))
}

#' @rdname autofilter
#' @export
autofilter.tblist <- function(source, attach_as = c("step", "meta"), ...) {
  attach_as <- rlang::arg_match(attach_as)
  description_obj <- source$description
  step_rule <- source$dtconn |>
    purrr::imap(~ filter_rules(.x, .y, description = description_obj[[.y]])) |>
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
