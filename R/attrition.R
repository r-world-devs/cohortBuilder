get_attrition_coords <- function(labels, n_included, space = 1, percent = FALSE) {
  n_total <- n_included[1]
  n_excluded <- stats::na.omit(n_included - dplyr::lead(n_included))

  label <- glue::glue("{labels}\nN = {n_included}")
  label_excl <- glue::glue("Excluded N = {n_excluded}")

  if (percent) {
    label <- glue::glue("{label} ({round(100 * n_included / n_total, 2)}%)")
    label_excl <- glue::glue("{label_excl} ({round(100 * n_excluded / n_total, 2)}%)")
  }
  label_excl <- c(NA, label_excl)
  dt <- data.frame(
    label = label,
    label_excl = label_excl
  ) %>%
    dplyr::mutate(
      label_heights = nchar(label) - nchar(gsub("\n", "", label)),
      label_position_y = dplyr::lag(cumsum(label_heights + space), default = 0),
      label_position_x = 0,
      arrow_end_position_y = dplyr::lead(label_position_y),
      excl_position_x = 0,
      excl_end_position_x = 40,
      excl_position_y = dplyr::lag(arrow_end_position_y) - (space / 2)
    )
  attr(dt, "space") <- space
  dt
}

get_attrition_plot <- function(attrition_coords) {
  max_y_lim <- max(attrition_coords$label_position_y) + max(attrition_coords$label_heights)
  space <- attr(attrition_coords, "space")
  if (is.null(space)) {
    space <- 1
  }
  ggplot2::ggplot(attrition_coords) +
    ggplot2::geom_segment(
      ggplot2::aes(x = label_position_x, xend = label_position_x, y = label_position_y, yend = arrow_end_position_y),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm")), na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = excl_position_x, xend = excl_end_position_x, y = excl_position_y, yend = excl_position_y),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm")), na.rm = TRUE
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = label, x = label_position_x, y = label_position_y),
      label.r = ggplot2::unit(0, "lines"), vjust = "top", size = 12/ggplot2::.pt, na.rm = TRUE
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = label_excl, x = excl_end_position_x, y = excl_position_y), label.r = ggplot2::unit(0, "lines"),
      hjust = "left", size = 12/ggplot2::.pt, na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(limits = c(max_y_lim + space, -space), trans = "reverse") +
    ggplot2::scale_x_continuous(limits = c(-35, 60)) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = NULL, y = NULL)
}

get_attrition_filter_label <- function(name, value_name, value) {
  if (is.list(value)) {
    value <- value %>%
      purrr::map_chr(~paste(., collapse = ",")) %>%
      purrr::imap(~paste(.y, " = ", .x)) %>%
      paste(collapse = ", ")
  } else if (is.vector(value)) {
    value <- paste(value, collapse = ", ")
  }
  glue::glue("Filter: {name} ({value_name} = [{value}])")
}

#' @param step_id Name of the step visible in resulting plot.
#' @param step_filters List of step filters.
#' @param ... Other parameters passed to specific S3 method.
#' @rdname source-layer
#' @export
.get_attrition_label <- function(source, step_id, step_filters, ...) {
  UseMethod(".get_attrition_label", source)
}

#' @rdname source-layer
#' @export
.get_attrition_label.default <- function(source, step_id, step_filters, ...) {
  pkey <- source$primary_keys
  binding_keys <- source$binding_keys
  if (step_id == "0") {
    if (is.null(pkey)) {
      return("Initial dataset")
    } else {
      dataset <- pkey[[1]]$dataset
      dataset_pkey <- pkey[[1]]$key
      return(glue::glue("{dataset}\n primary key: {paste(dataset_pkey, collapse = ', ')}"))
    }
  }
  filters_section <- step_filters %>%
    purrr::map(~get_attrition_filter_label(.$name, .$value_name, .$value)) %>%
    paste(collapse = "\n")
  bind_keys_section <- ""
  if (!is.null(binding_keys)) {
    dependent_datasets <- binding_keys %>%
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

#' @rdname source-layer
#' @param data_stats Data frame presenting statistics for each filtering step.
#' @param ... Other parameters passed to specific method.
#' @export
.get_attrition_count <- function(source, data_stats, ...) {
  UseMethod(".get_attrition_count", source)
}

#' @rdname source-layer
#' @export
.get_attrition_count.default <- function(source, data_stats, ...) {
  data_stats %>%
    purrr::map_int("n_rows")
}

