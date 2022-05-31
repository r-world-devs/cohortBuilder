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
#' @return Object of class 'tblist' being a named list of data frames.
#' @export
tblist <- function(..., names) {
  tables <- rlang::dots_list(..., .named = TRUE)
  out_class <- "tblist"

  tb_call <- sys.call(1)
  if (inherits(tables, "data.frame")) {
    if (!missing(names)) {
      names(tables) <- names
    }
    return(
      structure(tables, class = out_class)
    )
  }
  if (inherits(tables, "list")) {
    if (!missing(names)) {
      if (length(tables) != length(names)) {
        stop(glue::glue(
          "{sQuote('tables')} should be of same length as {sQuote('names')}"
        ))
      }
      return(
        structure(
          stats::setNames(tables, names),
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
as.tblist <- function(x, ...) {
  UseMethod("as.tblist", x)
}

#' @export
as.tblist.data.frame <- function(x, names, ...) {
  tblist(x, names = names)
}

#' @export
as.tblist.list <- function(x, names, ...) {
  tblist(!!!x, names = names)
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
          stats::na.omit() %>% table() %>% as.list(),
        n_data = if ("n_data" %in% name)  data_object[[dataset]][[variable]] %>%
          stats::na.omit() %>% length(),
        n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] %>% is.na() %>% sum()
      )
      if (length(name) == 1) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object) {
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][[variable]] %>% table %>% prop.table() %>% graphics::barplot()
      } else {
        graphics::barplot(0, ylim = c(0, 0.1), main = "No data")
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
            )[[1]]
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
        choices = if ("choices" %in% name) data_object[[dataset]][[variable]] %>% unique() %>% paste(collapse = ","),
        n_data = if ("n_data" %in% name)  data_object[[dataset]][[variable]] %>% stats::na.omit() %>% unique() %>% length(),
        n_missing = if ("n_missing" %in% name) data_object[[dataset]][[variable]] %>% is.na() %>% sum()
      )
      if (length(name) == 1) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object) {},
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
  step <- 1
  if (length(stats::na.omit(data_object[[dataset]][[variable]])) == 0) {
    return(
      data.frame(
        level = character(0),
        count = numeric(0),
        l_bound = numeric(0),
        u_bound = numeric(0)
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
        u_bound = max_val
      )
    )
  }

  if (!is.null(extra_params$step)) {
    step <- extra_params$step
  }
  breaks <- seq(min_val, max_val, by = step)
  if (rev(breaks)[1] != max_val) {
    breaks[length(breaks) + 1]  <- max_val
  }
  breaks <- round(breaks, 2)
  bounds <- breaks

  breaks[1] <- breaks[1] - 0.01
  breaks[length(breaks)] <- breaks[length(breaks)] + 0.01

  data_object[[dataset]][, variable, drop = FALSE] %>%
    dplyr::filter(!is.na(!!sym(variable))) %>%
    dplyr::mutate(
      level = factor(
        findInterval(!!sym(variable), breaks, rightmost.closed = FALSE),
        levels = 1:(length(breaks)),
        labels = as.character(1:(length(breaks)))
      )
    ) %>%
    dplyr::group_by(level) %>%
    dplyr::summarise(
      count = dplyr::n()
    ) %>%
    tidyr::complete(level, fill = list(count = 0)) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(
      l_bound = bounds,
      u_bound = c(bounds[-1], bounds[length(bounds)])
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
          dplyr::filter((!!sym(variable) <= !!range[2] & !!sym(variable) >= !!range[1]) | is.na(!!sym(variable)))
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
          dplyr::filter(!!sym(variable) <= !!range[2] & !!sym(variable) >= !!range[1])
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
      if (length(name) == 1) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object) {
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][[variable]] %>% graphics::hist()
      } else {
        graphics::barplot(0, ylim = c(0, 0.1), main = "No data")
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
          cache_object$frequencies$l_bound[1],
          rev(cache_object$frequencies$u_bound)[1]
        )
      )
    }
  )
}

get_date_range_frequencies <- function(data_object, dataset, variable, extra_params) {
  step <- "day"
  if (length(stats::na.omit(data_object[[dataset]][[variable]])) == 0) {
    return(
      data.frame(
        level = character(0),
        count = numeric(0),
        l_bound = numeric(0),
        u_bound = numeric(0)
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
        u_bound = max_val
      )
    )
  }

  if (!is.null(extra_params$step)) {
    step <- extra_params$step
  }
  breaks <- seq.Date(min_val, max_val, by = step)
  if (rev(breaks)[1] != max_val) {
    breaks[length(breaks) + 1]  <- max_val
  }

  data_object[[dataset]][, variable, drop = FALSE] %>%
    dplyr::filter(!is.na(!!sym(variable))) %>%
    dplyr::mutate(
      level = factor(
        findInterval(!!sym(variable), breaks, rightmost.closed = FALSE),
        levels = 1:(length(breaks)),
        labels = as.character(1:(length(breaks)))
      )
    ) %>%
    dplyr::group_by(level) %>%
    dplyr::summarise(
      count = dplyr::n()
    ) %>%
    tidyr::complete(level, fill = list(count = 0)) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(
      l_bound = breaks,
      u_bound = c(breaks[-1], breaks[length(breaks)])
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
          dplyr::filter((!!sym(variable) <= !!range[2] & !!sym(variable) >= !!range[1]) | is.na(!!sym(variable)))
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
          dplyr::filter(!!sym(variable) <= !!range[2] & !!sym(variable) >= !!range[1])
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
      if (length(name) == 1) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object) {
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][[variable]] %>% graphics::hist()
      } else {
        graphics::barplot(0, ylim = c(0, 0.1), main = "No data")
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
          cache_object$frequencies$l_bound[1],
          rev(cache_object$frequencies$u_bound)[1]
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
        } else if (is.null(value)){
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
          dplyr::across(
            !!names(values),
            ~col_in_val(.x, values[[dplyr::cur_column()]], !!keep_na)
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
      if (length(name) == 1) {
        return(stats[[name]])
      } else {
        return(stats[name])
      }
    },
    plot_data = function(data_object) {
      if (nrow(data_object[[dataset]])) {
        data_object[[dataset]][names(values)] %>%
          purrr::map(table) %>%
          purrr::imap_dfc(group_stats) %>%
          as.matrix() %>%
          graphics::barplot()
      } else {
        graphics::barplot(0, ylim = c(0, 0.1), main = "No data")
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
      data_object[[dataset]][[variables]]
    },
    get_defaults = function(data_object, cache_object) {
      list(values = names(cache_object$choices))
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
  common_key_names <- paste0("key_", seq_along(binding_key$data_keys[[1]]$key))
  for (dependent_dataset in dependent_datasets) {
    key_names <- binding_key$data_keys[[dependent_dataset]]$key
    tmp_key_values <- dplyr::distinct(data_object_post[[dependent_dataset]][, key_names, drop = FALSE]) %>%
      stats::setNames(common_key_names)
    if (is.null(key_values)) {
      key_values <- tmp_key_values
    } else {
      key_values <- dplyr::inner_join(key_values, tmp_key_values, by = common_key_names)
    }
  }

  data_object_post[[binding_dataset]] <- dplyr::inner_join(
    switch(
      as.character(binding_key$post),
      "FALSE" = data_object_pre[[binding_dataset]],
      "TRUE" = data_object_post[[binding_dataset]]
    ),
    key_values,
    by = stats::setNames(common_key_names, binding_key$update$key)
  )
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
      dataset_pkey <- .get_item(pkey, "dataset", dataset)[1][[1]]$key
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
      unique()
    if (length(dependent_datasets) > 0) {
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
