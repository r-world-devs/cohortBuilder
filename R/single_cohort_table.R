#' @param x Cohort
#' @param tables Vector of tables to be combined.
#'  The first is the main one on the basis of which the connection is created
#' @param main_key Name of primary key of main table
combine_tables <- function(x, tables, main_key, use_nest = TRUE) {
  source_cohort <- x$get_source()
  keys <- source_cohort$binding_keys
  combined_tables <- c()
  tables_keys <- list()

  # Rename columns "first_name" -> "actor_first_name"
  update_ds <- keys[[1]]$update$dataset
  data_keys_ds <- keys[[1]]$data_keys[[1]]$dataset


  if((update_ds %in% tables[-1]) && !(update_ds %in% unlist(tables_keys)) && use_nest){
    tables_keys <- append(tables_keys, list(c(dataset = update_ds, key = paste0(update_ds, "_", keys[[1]]$update$key))))
  }
  else if((data_keys_ds %in% tables[-1]) && !(data_keys_ds %in% unlist(tables_keys)) && use_nest){
    tables_keys <- append(tables_keys, list(c(dataset = data_keys_ds, key = paste0(data_keys_ds, "_", keys[[1]]$data_keys[[1]]$key))))
  }

  left_table <- get_data(x)[[update_ds]] %>%
    dplyr::rename_with(~paste0(update_ds,"_", .), everything())

  right_table <- get_data(x)[[data_keys_ds]] %>%
    dplyr::rename_with(~paste0(data_keys_ds,"_", .), everything())

  # Adding all tables to one big
  result <- left_table %>%
    dplyr::left_join(right_table, by = setNames(paste0(data_keys_ds, "_", keys[[1]]$data_keys[[1]]$key), paste0(update_ds, "_", keys[[1]]$update$key)), keep = TRUE)

  combined_tables <- append(combined_tables, c(update_ds, data_keys_ds))

  # Start on second bind key
  for(i in keys[2:length(keys)]) {
    update_ds <- i$update$dataset
    data_keys_ds <- i$data_keys[[1]]$dataset

    # Save name of key to create one table using a nested join
    if((update_ds %in% tables[-1]) && !(update_ds %in% unlist(tables_keys)) && use_nest){
      tables_keys <- append(tables_keys, list(c(dataset = update_ds, key = paste0(update_ds, "_", i$update$key))))
    }
    else if((data_keys_ds %in% tables[-1]) && !(data_keys_ds %in% unlist(tables_keys)) && use_nest){
      tables_keys <- append(tables_keys, list(c(dataset = data_keys_ds, key = paste0(data_keys_ds, "_", i$data_keys[[1]]$key))))
    }

    if((update_ds %in% combined_tables) && !(data_keys_ds %in% combined_tables)){
      right_table <- get_data(x)[[data_keys_ds]] %>%
        dplyr::rename_with(~paste0(data_keys_ds, "_", .), everything())

      result <- result %>%
        dplyr::left_join(right_table, by = setNames(paste0(data_keys_ds, "_", i$data_keys[[1]]$key ), paste0(update_ds,"_",i$update$key)), keep = TRUE)

      combined_tables <- append(combined_tables, data_keys_ds)
    }
    else if((data_keys_ds %in% combined_tables) && !(update_ds %in% combined_tables)) {
      right_table <- get_data(x)[[update_ds]] %>%
        dplyr::rename_with(~paste0(update_ds,"_", .), everything())

      result <- result %>%
        dplyr::left_join(right_table, by = setNames(paste0(update_ds,"_",i$update$key), paste0(data_keys_ds, "_", i$data_keys[[1]]$key )), keep = TRUE)

      combined_tables <- append(combined_tables, update_ds)
    }
  }

  if(use_nest){
    name_of_new_col <- paste0(tables[1], "_", tables_keys[[1]][[1]])
    main_key_result <- paste0(tables[1],"_",main_key)

    final_table <- get_data(x)[[tables[[1]]]] %>%
      dplyr::nest_join(dplyr::select(result, starts_with(tables_keys[[1]][[1]]), main_key_result),by = setNames(main_key_result, main_key), name = name_of_new_col)

    if(length(tables_keys) != 1) {
      for(i in 2:length(tables_keys)){
        name_of_new_col <- paste0(tables[1], "_", tables_keys[[i]][[1]])

        final_table <- final_table %>%
          dplyr::nest_join(dplyr::select(result, starts_with(tables_keys[[i]][[1]]), main_key_result), by =  setNames(main_key_result, main_key), name = name_of_new_col)
      }
    }
  }else{
    regex_pattern <- paste0("^(", paste(tables, collapse = "|"), ")")
    final_table <- dplyr::select(result,matches(regex_pattern))
  }

  return(final_table)
}
