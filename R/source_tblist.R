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
  dataset_names %>%
    purrr::map(
      ~ list(n_rows = nrow(data_object[[.x]]))
    ) %>%
    stats::setNames(dataset_names)
}

#' @rdname filter-source-types
#' @param dataset Dataset name to be used for filtering.
#' @param variable Dataset variable used for filtering.
#' @param value Value(s) to be used for filtering.
#' @param description Filter description (optional).
#' @param keep_na If `TRUE`, NA values are included.
#' @export
cb_filter.discrete.tblist <- function(
    source, type = "discrete", id = .gen_id(), name = id, variable, value = NA,
    dataset, keep_na = TRUE, ..., description = NULL, active = TRUE) {
  args <- list(...)

  def_filter(
    type = type,
    id = id,
    name = name,
    input_param = "value",
    filter_data = function(data_object) {
      if (keep_na && !identical(value, NA)) {
        # keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!!sym(variable) %in% !!c(value, NA))
        # keep_na !value_na end
      }
      if (!keep_na && identical(value, NA)) {
        # !keep_na value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!is.na(!!sym(variable)))
        # !keep_na value_na end
      }
      if (!keep_na && !identical(value, NA)) {
        # !keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!!sym(variable) %in% !!value)
        # !keep_na !value_na end
      }
      attr(data_object[[dataset]], "filtered") <- TRUE # code include
      data_object
    },
    get_stats = function(data_object, name) {
      if (missing(name)) {
        name <- c("n_data", "choices", "n_missing")
      }
      stats <- list(
        choices = if ("choices" %in% name) data_object[[dataset]][[variable]] %>%
          stats::na.omit() %>%
          table() %>%
          as.list(),
        n_data = if ("n_data" %in% name)  data_object[[dataset]][[variable]] %>% stats::na.omit() %>% length(),
        n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] %>% is.na() %>% sum()
      )
      if (length(name) == 1L) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object, ...) {
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][[variable]] %>% table %>% prop.table() %>% graphics::barplot(...)
      } else {
        graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
      }
    },
    get_params = function(name) {
      params <- list(
        dataset = dataset,
        variable = variable,
        value = value,
        keep_na = keep_na,
        description = description,
        active = active,
        ...
      )
      if (!missing(name)) return(params[[name]])
      return(params)
    },
    get_data = function(data_object) {
      data_object[[dataset]][[variable]]
    },
    get_defaults = function(data_object, cache_object) {
      list(value = names(cache_object$choices))
    }
  )
}

#' @rdname filter-source-types
#' @export
cb_filter.discrete_text.tblist <- function(
    source, type = "discrete_text", id = .gen_id(), name = id, variable, value = NA,
    dataset, ..., description = NULL, active = TRUE) {
  args <- list(...)

  def_filter(
    type = type,
    id = id,
    name = name,
    input_param = "value",
    filter_data = function(data_object) {

      if (!identical(value, NA)) {
        # keep_na !value_na start, # !keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(
            !!sym(variable) %in% !!strsplit(
              sub(" ", "", value, fixed = TRUE),
              split = ",", fixed = TRUE
            )[[1L]]
          )
        # keep_na !value_na end, # !keep_na !value_na end
      }
      attr(data_object[[dataset]], "filtered") <- TRUE # code include
      return(data_object)
    },
    get_stats = function(data_object, name) {
      if (missing(name)) {
        name <- c("n_data", "choices", "n_missing")
      }
      stats <- list(
        choices = if ("choices" %in% name) data_object[[dataset]][[variable]] %>%
          collapse::funique() %>%
          paste(collapse = ","),
        n_data = if ("n_data" %in% name)  data_object[[dataset]][[variable]] %>%
          stats::na.omit() %>%
          collapse::funique() %>%
          length(),
        n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] %>% is.na() %>% sum()
      )
      if (length(name) == 1L) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object, ...) {},
    get_params = function(name) {
      params <- list(
        dataset = dataset,
        variable = variable,
        value = value,
        description = description,
        active = active,
        ...
      )
      if (!missing(name)) return(params[[name]])
      return(params)
    },
    get_data = function(data_object) {
      data_object[[dataset]][[variable]]
    },
    get_defaults = function(data_object, cache_object) {
      list(value = cache_object$choices)
    }
  )
}

get_range_frequencies <- function(data_object, dataset, variable, extra_params) {
  step <- 1L
  if (length(stats::na.omit(data_object[[dataset]][[variable]])) == 0L) {
    return(
      data.frame(
        level = character(0L),
        count = numeric(0L),
        l_bound = numeric(0L),
        u_bound = numeric(0L),
        stringsAsFactors = FALSE
      )
    )
  }
  min_val <- min(data_object[[dataset]][[variable]], na.rm = TRUE)
  max_val <- max(data_object[[dataset]][[variable]], na.rm = TRUE)
  if (min_val == max_val) {
    return(
      data.frame(
        level = "1",
        count = length(data_object[[dataset]][[variable]]),
        l_bound = min_val,
        u_bound = max_val,
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(extra_params$step)) {
    step <- extra_params$step
  }
  breaks <- seq(min_val, max_val, by = step)
  if (rev(breaks)[1L] != max_val) {
    breaks[length(breaks) + 1L]  <- max_val
  }
  breaks <- round(breaks, 2L)
  bounds <- breaks

  breaks[1L] <- breaks[1L] - 0.01
  breaks[length(breaks)] <- breaks[length(breaks)] + 0.01

  data_object[[dataset]][, variable, drop = FALSE] %>%
    dplyr::filter(!is.na(!!sym(variable))) %>%
    dplyr::mutate(
      level = factor(
        findInterval(!!sym(variable), breaks, rightmost.closed = FALSE),
        levels = seq_along(breaks),
        labels = as.character(seq_along(breaks))
      )
    ) %>%
    dplyr::group_by(level) %>%
    dplyr::summarise(
      count = dplyr::n()
    ) %>%
    tidyr::complete(level, fill = list(count = 0L)) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(
      l_bound = bounds,
      u_bound = c(bounds[-1L], bounds[length(bounds)])
    )
}

#' @rdname filter-source-types
#' @param range Variable range to be applied in filtering.
#' @export
cb_filter.range.tblist <- function(
    source, type = "range", id = .gen_id(), name = id, variable, range = NA, dataset,
    keep_na = TRUE, ..., description = NULL, active = TRUE) {
  args <- list(...)

  def_filter(
    type = type,
    id = id,
    name = name,
    input_param = "range",
    filter_data = function(data_object) {

      if (keep_na && !identical(range, NA)) {
        # keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter((!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L]) | is.na(!!sym(variable)))
        # keep_na !value_na end
      }
      if (!keep_na && identical(range, NA)) {
        # !keep_na value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!is.na(!!sym(variable)))
        # !keep_na value_na end
      }
      if (!keep_na && !identical(range, NA)) {
        # !keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L])
        # !keep_na !value_na end
      }
      attr(data_object[[dataset]], "filtered") <- TRUE # code include
      return(data_object)
    },
    get_stats = function(data_object, name) {
      if (missing(name)) {
        name <- c("n_data", "frequencies", "n_missing")
      }
      extra_params <- list(...)

      stats <- list(
        frequencies = if ("frequencies" %in% name) {
          get_range_frequencies(data_object, dataset, variable, extra_params)
        },
        n_data = if ("n_data" %in% name)  data_object[[dataset]][[variable]] %>% stats::na.omit() %>% length(),
        n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] %>% is.na() %>% sum()
      )
      if (length(name) == 1L) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object, ...) {
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][[variable]] %>% graphics::hist(...)
      } else {
        graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
      }
    },
    get_params = function(name) {
      params <- list(
        dataset = dataset,
        variable = variable,
        range = range,
        keep_na = keep_na,
        description = description,
        active = active,
        ...
      )
      if (!missing(name)) return(params[[name]])
      return(params)
    },
    get_data = function(data_object) {
      data_object[[dataset]][[variable]]
    },
    get_defaults = function(data_object, cache_object) {
      list(
        range = c(
          cache_object$frequencies$l_bound[1L],
          rev(cache_object$frequencies$u_bound)[1L]
        )
      )
    }
  )
}

get_date_range_frequencies <- function(data_object, dataset, variable, extra_params) {
  step <- "day"
  if (length(stats::na.omit(data_object[[dataset]][[variable]])) == 0L) {
    return(
      data.frame(
        level = character(0L),
        count = numeric(0L),
        l_bound = numeric(0L),
        u_bound = numeric(0L),
        stringsAsFactors = FALSE
      )
    )
  }
  min_val <- min(data_object[[dataset]][[variable]], na.rm = TRUE)
  max_val <- max(data_object[[dataset]][[variable]], na.rm = TRUE)
  if (min_val == max_val) {
    return(
      data.frame(
        level = "1",
        count = length(data_object[[dataset]][[variable]]),
        l_bound = min_val,
        u_bound = max_val,
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(extra_params$step)) {
    step <- extra_params$step
  }
  breaks <- seq.Date(min_val, max_val, by = step)
  if (rev(breaks)[1L] != max_val) {
    breaks[length(breaks) + 1L]  <- max_val
  }

  data_object[[dataset]][, variable, drop = FALSE] %>%
    dplyr::filter(!is.na(!!sym(variable))) %>%
    dplyr::mutate(
      level = factor(
        findInterval(!!sym(variable), breaks, rightmost.closed = FALSE),
        levels = seq_along(breaks),
        labels = as.character(seq_along(breaks))
      )
    ) %>%
    dplyr::group_by(level) %>%
    dplyr::summarise(
      count = dplyr::n()
    ) %>%
    tidyr::complete(level, fill = list(count = 0L)) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(
      l_bound = breaks,
      u_bound = c(breaks[-1L], breaks[length(breaks)])
    )
}

#' @rdname filter-source-types
#' @export
cb_filter.date_range.tblist <- function(
    source, type = "date_range", id = .gen_id(), name = id, variable, range = NA,
    dataset, keep_na = TRUE, ..., description = NULL, active = TRUE) {
  args <- list(...)

  def_filter(
    type = type,
    id = id,
    name = name,
    input_param = "range",
    filter_data = function(data_object) {

      if (keep_na && !identical(range, NA)) {
        # keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter((!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L]) | is.na(!!sym(variable)))
        # keep_na !value_na end
      }
      if (!keep_na && identical(range, NA)) {
        # !keep_na value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!is.na(!!sym(variable)))
        # !keep_na value_na end
      }
      if (!keep_na && !identical(range, NA)) {
        # !keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!!sym(variable) <= !!range[2L] & !!sym(variable) >= !!range[1L])
        # !keep_na !value_na end
      }
      attr(data_object[[dataset]], "filtered") <- TRUE # code include
      return(data_object)
    },
    get_stats = function(data_object, name) {
      if (missing(name)) {
        name <- c("n_data", "frequencies", "n_missing")
      }
      extra_params <- list(...)

      stats <- list(
        frequencies = if ("frequencies" %in% name) {
          get_date_range_frequencies(data_object, dataset, variable, extra_params)
        },
        n_data = if ("n_data" %in% name)  data_object[[dataset]][[variable]] %>% stats::na.omit() %>% length(),
        n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] %>% is.na() %>% sum()
      )
      if (length(name) == 1L) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object, ..., breaks) {
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][[variable]] %>% graphics::hist(..., breaks = breaks)
      } else {
        graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
      }
    },
    get_params = function(name) {
      params <- list(
        dataset = dataset,
        variable = variable,
        range = range,
        keep_na = keep_na,
        description = description,
        active = active,
        ...
      )
      if (!missing(name)) return(params[[name]])
      return(params)
    },
    get_data = function(data_object) {
      data_object[[dataset]][[variable]]
    },
    get_defaults = function(data_object, cache_object) {
      list(
        range = c(
          cache_object$frequencies$l_bound[1L],
          rev(cache_object$frequencies$u_bound)[1L]
        )
      )
    }
  )
}

col_choices <- function(vec) {
  counts <- table(vec)
  stats::setNames(names(counts), paste(names(counts), glue::glue("({counts})")))
}

group_stats <- function(vec_stats, name) {
  data.frame(val = as.vector(vec_stats), row.names = names(vec_stats)) %>%
    stats::setNames(name)
}

calculate_datetime_step <- function(min_date, max_date) {
  # Define possible steps
  steps <- c(
    "mins" = 60L,
    "hours" = 3600L,
    "days" = 86400L,
    "weeks" = 604800L,
    "months" = 2592000L,
    "years" = 31104000L
  )
  time_span <- as.numeric(max_date) - as.numeric(min_date)
  num_elements <- as.integer(time_span / steps)
  idx <- which(num_elements <= 200L)[1L]
  if (!is.na(idx)) {
    return(steps[idx])
  }

  return(steps[length(steps)])
}

#' @rdname filter-source-types
#' @export
cb_filter.datetime_range.tblist <- function(
    source, type = "datetime_range", id = .gen_id(), name = id, variable, range = NA,
    dataset, keep_na = TRUE, ..., description = NULL, active = TRUE) {

  args <- list(...)

  def_filter(
    type = type,
    id = id,
    name = name,
    input_param = "range",
    filter_data = function(data_object) {
      if (keep_na && !identical(range, NA)) {
        # keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(
            (!!sym(variable) >= !!range[1L] & !!sym(variable) <= !!range[2L]) |
              is.na(!!sym(variable))
          )
        # keep_na !value_na end
      }
      if (!keep_na && identical(range, NA)) {
        # !keep_na value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!is.na(!!sym(variable)))
        # !keep_na value_na end
      }
      if (!keep_na && !identical(range, NA)) {
        # !keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(
            !!sym(variable) >= !!range[1L] & !!sym(variable) <= !!range[2L]
          )
        # !keep_na !value_na end
      }

      attr(data_object[[dataset]], "filtered") <- TRUE
      return(data_object)
    },
    get_stats = function(data_object, name) {
      if (missing(name)) {
        name <- c("n_data", "frequencies", "n_missing")
      }
      extra_params <- list(...)

      data_object[[dataset]][[variable]] <- as.numeric(data_object[[dataset]][[variable]])

      if (is.null(extra_params$step) && !identical(length(data_object[[dataset]][[variable]]), 0L)) {
        min <- min(data_object[[dataset]][[variable]], na.rm = TRUE)
        max <- max(data_object[[dataset]][[variable]], na.rm = TRUE)

        extra_params$step <- calculate_datetime_step(min, max) %>% unname()
      }

      stats <- list(
        frequencies = if ("frequencies" %in% name) {
          get_range_frequencies(data_object, dataset, variable, extra_params)
        },
        n_data = if ("n_data" %in% name) {
          data_object[[dataset]][[variable]] %>% stats::na.omit() %>% length()
        },
        n_missing = if ("n_missing" %in% name) {
          data_object[[dataset]][[variable]] %>% is.na() %>% sum()
        }
      )

      if (length(name) == 1L) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object, ..., breaks = NULL) {
      if (nrow(data_object[[dataset]])) {
        if (is.null(breaks)) {
          breaks <- calculate_datetime_step(
            min(data_object[[dataset]][[variable]], na.rm = TRUE),
            max(data_object[[dataset]][[variable]], na.rm = TRUE)
          ) %>% names()
        }

        data_object[[dataset]][[variable]] %>%
          graphics::hist(..., breaks = breaks)
      } else {
        graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
      }
    },
    get_params = function(name) {
      params <- list(
        dataset = dataset,
        variable = variable,
        range = range,
        keep_na = keep_na,
        description = description,
        active = active,
        ...
      )
      if (!missing(name)) return(params[[name]])
      return(params)
    },
    get_data = function(data_object) {
      data_object[[dataset]][[variable]]
    },
    get_defaults = function(data_object, cache_object) {
      list(
        range = c(
          cache_object$frequencies$l_bound[1L],
          rev(cache_object$frequencies$u_bound)[1L]
        )
      )
    }
  )
}

#' @rdname filter-source-types
#' @param variables Vector of variable names to be used in filtering.
#' @param values Named list of values to be applied in filtering.
#'   The names should relate to the ones included in `variables` parameter.
#' @export
cb_filter.multi_discrete.tblist <- function(
    source, type = "multi_discrete", id = .gen_id(), name = id, values,
    variables, dataset, keep_na = TRUE, ..., description = NULL, active = TRUE) {
  args <- list(...)

  def_filter(
    type = type,
    id = id,
    name = name,
    input_param = "values",
    filter_data = function(data_object) {

      # code include start
      col_in_val <- function(vec, value, keep_na) {
        if (identical(value, NA)) {
          val_mask <- rep(TRUE, length(vec))
        } else if (is.null(value)) {
          val_mask <- rep(FALSE, length(vec))
        } else {
          val_mask <- vec %in% value
        }
        if (keep_na) {
          return(is.na(vec) | val_mask)
        } else {
          return(!is.na(vec) & val_mask)
        }
      }

      data_object[[dataset]] <- data_object[[dataset]] %>%
        dplyr::filter(
          dplyr::if_all(
            dplyr::all_of(names(values)),
            # Using deparse(substitute(.x)) over dplyr::cur_column
            # dplyr::cur_column is accessible only in across
            ~ col_in_val(.x, values[[deparse(substitute(.x))]], !!keep_na)
          )
        )
      attr(data_object[[dataset]], "filtered") <- TRUE
      # code include end
      return(data_object)
    },
    get_stats = function(data_object, name) {
      if (missing(name)) {
        name <- c("n_data", "choices", "n_missing")
      }
      variables <- unlist(variables)
      stats <- list(
        choices = if ("choices" %in% name) data_object[[dataset]][variables] %>% purrr::map(~as.list(table(.))),
        n_data = if ("n_data" %in% name)  data_object[[dataset]][variables] %>% nrow(),
        n_missing = if ("n_missing" %in% name) data_object[[dataset]][variables] %>% is.na() %>% colSums() %>% as.list()
      )
      if (length(name) == 1L) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object, ...) {
      variables <- unlist(variables)
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][variables] %>%
          purrr::map(table) %>%
          purrr::imap_dfc(group_stats) %>%
          as.matrix() %>%
          graphics::barplot(...)
      } else {
        graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
      }
    },
    get_params = function(name) {
      params <- list(
        dataset = dataset,
        variables = variables,
        values = values,
        keep_na = keep_na,
        description = description,
        active = active,
        ...
      )
      if (!missing(name)) return(params[[name]])
      return(params)
    },
    get_data = function(data_object) {
      data_object[[dataset]][, variables]
    },
    get_defaults = function(data_object, cache_object) {
      list(values = names(cache_object$choices))
    }
  )
}

#' @rdname filter-source-types
#' @param dataset Dataset name to be used for filtering.
#' @param variables Dataset variables used for filtering.
#' @param value Value(s) to be used for filtering.
#' @param description Filter description (optional).
#' @param keep_na If `TRUE`, NA values are included.
#' @export
cb_filter.query.tblist <- function(
    source, type = "query", id = .gen_id(), name = id, variables, value = NA,
    dataset, keep_na = TRUE, ..., description = NULL, active = TRUE) {
  args <- list(...)

  def_filter(
    type = type,
    id = id,
    name = name,
    input_param = "value",
    filter_data = function(data_object) {
      if (keep_na && !identical(value, NA)) {
        # keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!!queryBuilder::queryToExpr(value, keep_na = keep_na))
        # keep_na !value_na end
      }
      if (!keep_na && !identical(value, NA)) {
        # !keep_na !value_na start
        data_object[[dataset]] <- data_object[[dataset]] %>%
          dplyr::filter(!!queryBuilder::queryToExpr(value))
        # !keep_na !value_na end
      }
      attr(data_object[[dataset]], "filtered") <- TRUE # code include
      data_object
    },
    get_stats = function(data_object, name) {
      if (missing(name)) {
        name <- c("n_data", "specs", "n_missing")
      }
      variables <- unlist(variables)
      stat_from_column <- base::get("stat_from_column", envir = asNamespace("queryBuilder"), inherits = FALSE)
      stats <- list(
        specs = if ("specs" %in% name) data_object[[dataset]][variables] %>% purrr::imap(stat_from_column),
        n_data = if ("n_data" %in% name)  data_object[[dataset]][variables] %>% nrow(),
        n_missing = if ("n_missing" %in% name) data_object[[dataset]][variables] %>% is.na() %>% colSums() %>% as.list()
      )
      if (length(name) == 1L) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object, ...) {
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][variables] %>%
          purrr::map(table) %>%
          purrr::imap_dfc(group_stats) %>%
          as.matrix() %>%
          graphics::barplot(...)
      } else {
        graphics::barplot(0.0, ylim = c(0.0, 0.1), main = "No data")
      }
    },
    get_params = function(name) {
      params <- list(
        dataset = dataset,
        variables = variables,
        value = value,
        keep_na = keep_na,
        description = description,
        active = active,
        ...
      )
      if (!missing(name)) return(params[[name]])
      return(params)
    },
    get_data = function(data_object) {
      data_object[[dataset]][, variables, drop = FALSE]
    },
    get_defaults = function(data_object, cache_object) {
      list(value = names(cache_object$choices))
    }
  )
}

#' @export
.run_binding.tblist <- function(source, binding_key, data_object_pre, data_object_post, ...) {
  binding_dataset <- binding_key$update$dataset
  dependent_datasets <- names(binding_key$data_keys)
  active_datasets <- data_object_post %>%
    purrr::keep(~ attr(., "filtered")) %>%
    names()

  if (!any(dependent_datasets %in% active_datasets)) {
    return(data_object_post)
  }

  key_values <- NULL
  common_key_names <- paste0("key_", seq_along(binding_key$data_keys[[1L]]$key))
  for (dependent_dataset in dependent_datasets) {
    key_names <- binding_key$data_keys[[dependent_dataset]]$key
    tmp_key_values <- collapse::funique(data_object_post[[dependent_dataset]][, key_names, drop = FALSE]) %>%
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
      df,
      key_values,
      on = stats::setNames(common_key_names, binding_key$update$key),
      how = "inner",
      verbose = getOption("cb_verbose", default = FALSE)
    )
  }, error = function(e) {
    dplyr::inner_join(
      df,
      key_values,
      by = stats::setNames(common_key_names, binding_key$update$key)
    )
  })

  if (binding_key$activate) {
    attr(data_object_post[[binding_dataset]], "filtered") <- TRUE
  }

  return(data_object_post)
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
  filters_section <- step_filters %>%
    purrr::keep(~.$dataset == dataset) %>%
    purrr::map(~get_attrition_filter_label(.$name, .$value_name, .$value)) %>%
    paste(collapse = "\n")
  bind_keys_section <- ""
  if (!is.null(binding_keys)) {
    dependent_datasets <- .get_item(
      binding_keys, attribute = "update", value = dataset,
      operator = function(value, target) {
        value == target$dataset
      }
    ) %>%
      purrr::map(~names(.[["data_keys"]])) %>%
      unlist() %>%
      collapse::funique()
    if (length(dependent_datasets) > 0L) {
      bind_keys_section <- glue::glue(
        "\nData linked with external datasets: {paste(dependent_datasets, collapse = ', ')}",
        .trim = FALSE
      )
    }
  }
  gsub(
    "\n$",
    "",
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
  data_stats %>%
    purrr::map_int(~.[[dataset]][["n_rows"]])
}

#' @export
.pre_filtering.tblist <- function(source, data_object, step_id) {
  for (dataset in names(data_object)) {
    attr(data_object[[dataset]], "filtered") <- FALSE
  }
  return(data_object)
}

#' @export
.repro_code_tweak.tblist <- function(source, code_data) {
  pipe_all_filters(code_data)
}
