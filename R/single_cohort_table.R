#' @param x Cohort
#' @param tables Vector of tables to be combined.
#'  The first is the main one on the basis of which the connection is created
combine_tables <- function(x, tables, use_nest = TRUE) {
  if (!all(tables %in% names(get_data(x)))) {
    stop("one or more tables do not appear in this cohort")
  }

  source_cohort <- x$get_source()
  prim_keys <- source_cohort$primary_keys
  keys <- source_cohort$binding_keys
  tables_keys <- list()
  update_ds <- keys[[1]]$update$dataset

  subset_tables <- create_subset_list(keys)
  tables_in_subset <- get_tables_from_subsets(tables, subset_tables)

  if (is.null(tables_in_subset)) {
    stop("At this moment we do not support joining 2 tables from different table subsets")
  }

  keys <- keys %>%
    purrr::keep(~ .x$update$dataset %in% tables_in_subset &
                  .x$data_key[[1]]$dataset %in% tables_in_subset)

  main_key <- prim_keys %>%
    purrr::keep(~.x$dataset == tables[1]) %>%
    purrr::map_chr("key") %>%
    dplyr::first()

  keys <- select_keys(tables, keys)

  final_table <- join_tables(x, keys, use_nest, main_key, tables)

  return(final_table)
}

#' @param keys List of binding keys
create_subset_list <- function(keys) {
  subset_list <- list("subset_1" = keys[[1]]$update$dataset)
  id_subset_list <- 2

  purrr::walk(keys, \(key){
    found <- FALSE
    up_table <- key$update$dataset
    data_table <- key$data_keys[[1]]$dataset

    matching_subsets <- subset_list %>%
      purrr::keep(~ up_table %in% .x)
    if (!(rlang::is_empty(matching_subsets))) {
      subset_list[names(matching_subsets)] <- matching_subsets %>%
        purrr::map(~ if (!(data_table %in% .x)){
          append(.x, data_table)
        }else .x)
      found <- TRUE
    }

    matching_subsets <- subset_list %>%
      purrr::keep(~ data_table %in% .x)
    if (!(rlang::is_empty(matching_subsets))) {
      subset_list[names(matching_subsets)] <- matching_subsets %>%
        purrr::map(~ if (!(up_table %in% .x)){
          append(.x, up_table)
        }else .x)
      found <- TRUE
    }

    if (!found) {
      subset_list[[paste0("subset_", id_subset_list)]] <- c(up_table, data_table)
      id_subset_list <- id_subset_list + 1
    }
  })

  return(subset_list)
}

#' @param tables vector of tables to check in all subset and merged subsets
#' @param subset_tables list of subsets
#' @return vector of tables from subset where all tables from input are located
get_tables_from_subsets <- function(tables, subset_tables) {
  subset <- subset_tables %>%
    purrr::keep(~all(tables %in% .x))

  if(!rlang::is_empty(subset)){
    return(subset)
  }

  if (length(subset_tables) == 1) {
    return(NULL)
  }

  change <- FALSE
  start_id <- 1
  while (TRUE) {
    end_id <- length(subset_tables)

    if (change) {
      if (all(tables %in% subset_tables[[start_id]])) {
        return(subset_tables[[start_id]])
      }
      change <- FALSE
    }

    purrr::walk((start_id + 1):end_id, function(id) {
      common_tables <- intersect(subset_tables[[start_id]], subset_tables[[id]])
      if (length(common_tables) > 0) {
        subset_tables[[start_id]] <- union(subset_tables[[start_id]], subset_tables[[id]])
        subset_tables[[id]] <- NULL
        change <<- TRUE
        return(NULL)
      }
    })


    if (!change) {
      start_id <- start_id + 1
    }
    if (start_id >= end_id - 1) {
      return(NULL)
    }
  }
}

join_tables <- function(x, keys, use_nest, main_key, tables) {
  tables_keys <- list()

  update_ds <- keys[[1]]$update$dataset

  if (update_ds %in% tables[-1]) {
    tables_keys <- append(tables_keys, list(c(
      dataset = update_ds,
      key = paste0(update_ds, "_", keys[[1]]$update$key)
    )))
  }

  # Rename columns "first_name" -> "actor_first_name"
  result <- get_data(x)[[update_ds]] %>%
    dplyr::rename_with( ~ paste0(update_ds, "_", .), everything())

  combined_tables <- update_ds

  for (i in keys) {
    update_ds <- i$update$dataset
    data_keys_ds <- i$data_keys[[1]]$dataset

    # Save name of key to create one table using a nested join
    if ((update_ds %in% tables[-1]) &&
        !(update_ds %in% unlist(tables_keys))) {
      tables_keys <- tables_keys %>%
        append(list(c(
          dataset = update_ds,
          key = paste0(update_ds, "_", i$update$key)
        )))
    } else if ((data_keys_ds %in% tables[-1]) &&
               !(data_keys_ds %in% unlist(tables_keys))) {
      tables_keys <- tables_keys %>%
        append(list(c(
          dataset = data_keys_ds,
          key = paste0(data_keys_ds, "_", i$data_keys[[1]]$key)
        )))
    }

    if ((update_ds %in% combined_tables) &&
        !(data_keys_ds %in% combined_tables)) {
      right_table <- get_data(x)[[data_keys_ds]] %>%
        dplyr::rename_with( ~ paste0(data_keys_ds, "_", .), everything())

      result <- result %>%
        dplyr::left_join(
          right_table,
          by = rlang::set_names(
            paste0(data_keys_ds, "_", i$data_keys[[1]]$key),
            paste0(update_ds, "_", i$update$key)
          ),
          keep = TRUE,
          relationship = "many-to-many"
        )

      combined_tables <- append(combined_tables, data_keys_ds)
    } else if ((data_keys_ds %in% combined_tables) &&
               !(update_ds %in% combined_tables)) {
      right_table <- get_data(x)[[update_ds]] %>%
        dplyr::rename_with( ~ paste0(update_ds, "_", .), everything())

      result <- result %>%
        dplyr::left_join(
          right_table,
          by = rlang::set_names(
            paste0(update_ds, "_", i$update$key),
            paste0(data_keys_ds, "_", i$data_keys[[1]]$key)
          ),
          keep = TRUE,
          relationship = "many-to-many"
        )

      combined_tables <- append(combined_tables, update_ds)
    }
  }
  if (use_nest) {
    name_of_new_col <- paste0(tables[1], "_", tables_keys[[1]][[1]])
    main_key_result <- paste0(tables[1], "_", main_key)
    # Name of table to combine
    table_name <- tables_keys[[1]][[1]]
    # Names of columns in table to combine
    col_names <- names(get_data(x)[[table_name]])

    final_table <- get_data(x)[[tables[[1]]]] %>%
      dplyr::nest_join(
        result %>% dplyr::select(paste0(table_name, "_", col_names), main_key_result),
        by = rlang::set_names(main_key_result, main_key),
        name = name_of_new_col
      )

    final_table[[name_of_new_col]] <- lapply(final_table[[name_of_new_col]], function(df) {
      df %>% dplyr::distinct(across(all_of(tables_keys[[1]][[2]])), .keep_all = TRUE)
    })

    if (!(length(tables_keys) < 2)) {
      for (i in 2:length(tables_keys)) {
        name_of_new_col <- paste0(tables[1], "_", tables_keys[[i]][[1]])
        # Name of table to combine
        table_name <- tables_keys[[i]][[1]]
        # Names of columns in table to combine
        col_names <- names(get_data(x)[[table_name]])

        final_table <- final_table %>%
          dplyr::nest_join(
            result %>% dplyr::select(paste0(table_name, "_", col_names), main_key_result),
            by = rlang::set_names(main_key_result, main_key),
            name = name_of_new_col
          )

        final_table[[name_of_new_col]] <- lapply(final_table[[name_of_new_col]], function(df) {
          df %>% dplyr::distinct(across(all_of(tables_keys[[i]][[2]])), .keep_all = TRUE)
        })
      }
    }
  } else {
    regex_pattern <- paste0("^(", paste(tables, collapse = "|"), ")")
    final_table <- dplyr::select(result, matches(regex_pattern))
  }

  return(final_table)
}

select_keys <- function(tables, keys) {
  id <- 1
  keys_test <- keys
  continue <- TRUE
  while (continue) {
    keys_test <- keys[-id]
    subset_tables <- create_subset_list(keys_test)
    tables_in_subset <- get_tables_from_subsets(tables, subset_tables)

    if (!is.null(tables_in_subset)) {
      keys <- keys %>%
        purrr::keep(~ .x$update$dataset %in% tables_in_subset &
                      .x$data_key[[1]]$dataset %in% tables_in_subset)
    } else {
      id <- id + 1
    }
    if (id > length(keys)) {
      continue <- FALSE
    }
  }

  return(keys)
}

#' @description
#' Create plot to draw connection between binding keys
#'
#' @param x Cohort
plot_binding_keys <- function(x) {
  source_cohort <- x$get_source()

  edges <- do.call(rbind, lapply(source_cohort$binding_keys, function(bind) {
    source <- bind$update$dataset
    targets <- purrr::map_vec(bind$data_keys, function(dk)
      dk$dataset)
    data.frame(source = source,
               target = targets,
               stringsAsFactors = FALSE)
  }))

  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

  ggraph::ggraph(graph, layout = "fr") +
    ggraph::geom_edge_link(linewidth = 0.8) +
    ggraph::geom_node_point(size = 6, color = "blue") +
    ggraph::geom_node_text(ggplot2::aes(label = name),
                           vjust = 1.5,
                           size = 5) +
    ggplot2::theme_minimal()
}
