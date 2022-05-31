#' R6 class representing Cohort object.
#'
#' Cohort object is designed to make operations on source data possible.
#' @param source Source object created with \link{set_source}.
#' @param run_flow If `TRUE`, data flow is run after the operation is completed.
#' @param hook List of hooks describing methods before/after the Cohort is created.
#'     See \link{hooks} for more details.
Cohort <- R6::R6Class(
  "Cohort",
  public = list(
    #' @description
    #' Create Cohort object.
    #' @param ... Steps definition (optional). Can be also defined as a sequence of
    #'     filters - the filters will be added to the first step.
    #' @return The object of class `Cohort`.
    initialize = function(source, ..., run_flow = FALSE,
                          hook = list(
                            pre = get_hook("pre_cohort_hook"),
                            post = get_hook("post_cohort_hook")
                          )) {
      run_hooks(hook$pre, self, private)

      if (!missing(source)) {
        private$init_source(source, ...)

        run_hooks(hook$post, self, private)

        if (run_flow) {
          self$run_flow()
        }
      }
    },
    #' @description
    #' Add Source to Cohort object.
    add_source = function(source) {
      private$init_source(source)
    },
    #' @description
    #' Update Source in the Cohort object.
    #' @param keep_steps If `TRUE`, steps definition remains unchanged when updating source.
    #'    If `FALSE` steps configuration is deleted.
    #'    If vector of type integer, specified steps will remain.
    update_source = function(source, keep_steps = !has_steps(source), run_flow = FALSE,
                             hook = list(
                               pre = get_hook("pre_update_source_hook"),
                               post = get_hook("post_update_source_hook")
                             )) {
      run_hooks(hook$pre, self, private, keep_steps)

      private$data_objects <- list()
      private$source <- NULL
      private$cache <- NULL

      if (identical(keep_steps, FALSE)) {
        private$steps <- list()
        private$init_source(source)
      } else {
        state <- self$get_state(json = FALSE)
        if (!isTRUE(keep_steps)) {
          state <- state[as.integer(keep_steps)]
        }
        steps <- list()
        for (step_state in state) {
          steps[[step_state$step]] <- do.call(
            step,
            step_state$filters %>% purrr::map(~do.call(filter, .))
          )
        }
        do.call(private$init_source, append(list(source = source), steps))
      }

      run_hooks(hook$post, self, private, keep_steps)

      if (run_flow) {
        self$run_flow()
      }
    },
    #' @description
    #' Return Source object attached to Cohort.
    get_source = function() {
      private$source
    },
    #' @description
    #' Add filtering step definition
    #' @param step Step definition created with \link{step}.
    add_step = function(step, run_flow = FALSE,
                        hook = list(
                          pre = get_hook("pre_add_step_hook"),
                          post = get_hook("post_add_step_hook")
                        )) {

      new_step_id <- as.character(as.integer(self$last_step_id()) + 1)

      run_hooks(hook$pre, self, private, new_step_id)

      private$steps[new_step_id] <- step %>%
        attach_step_id(new_step_id) %>%
        list() %>%
        purrr::map(eval_step_filters, source = private$source)
      names(private$steps[new_step_id]) <- new_step_id

      run_hooks(hook$post, self, private, new_step_id)

      if (run_flow) {
        self$run_flow(min_step = new_step_id)
      }
    },
    #' @description
    #' Copy selected step.
    #' @param step_id Id of the step to be copied. If missing the last step is taken.
    #' The copied step is added as the last one in the Cohort.
    #' @param filters List of Source-evaluated filters to copy to new step.
    copy_step = function(step_id, filters, run_flow = FALSE) {
      if (missing(step_id)) {
        step_id <- self$last_step_id()
      }
      if (!missing(filters)) {
        step_id <- self$last_step_id()
        step_config <- list(
          step = next_step(step_id),
          filters = purrr::map(filters, get_filter_state, extra_fields = NULL)
        )
      } else {
        step_config <- self$get_state(step_id, json = FALSE)[[1]]
        step_config$step <- next_step(step_id)
      }

      self$add_step(
        do.call(
          step,
          step_config$filters %>% purrr::map(~do.call(filter, .))
        )
      )
      if (run_flow) {
        self$run_flow(min_step = step_config$step)
      }
    },
    #' @description
    #' Remove filtering step definition
    #' @param step_id Id of the step to remove.
    remove_step = function(step_id, run_flow = FALSE,
                           hook = list(
                             pre = get_hook("pre_rm_step_hook"),
                             post = get_hook("post_rm_step_hook")
                           )) {

      if (missing(step_id)) {
        step_id <- self$last_step_id()
      }
      run_hooks(hook$pre, self, private, step_id)

      step_id <- as.character(step_id)
      private$steps[[step_id]] <- NULL
      private$cache[[step_id]] <- NULL
      private$data_objects[steps_range(step_id, length(private$data_objects))] <- NULL
      private$steps <- adjust_names(private$steps)
      if (!is.null(private$steps) && run_flow) {
        self$run_flow(min_step = step_id)
      }

      run_hooks(hook$post, self, private, step_id)
    },
    #' @description
    #' Add filter definition
    #' @param filter Filter definition created with \link{filter}.
    #' @param step_id Id of the step to add the filter to.
    #'     If missing, filter is added to the last step.
    add_filter = function(filter, step_id, run_flow = FALSE) {
      if (missing(step_id)) {
        step_id <- self$last_step_id()
        if (step_id == "0") {
          step_id <- "1"
        }
      }
      step_id <- as.character(step_id)
      evaled_filter <- eval_filter(filter, step_id, private$source)
      private$steps[[step_id]]$filters[[evaled_filter$id]] <- evaled_filter
      private$steps[[step_id]]$id <- step_id
      if (run_flow) {
        self$run_flow(min_step = step_id)
      }
    },
    #' @description
    #' Remove filter definition
    #' @param step_id Id of the step from which filter should be removed.
    #' @param filter_id Id of the filter to be removed.
    remove_filter = function(step_id, filter_id, run_flow = FALSE) {
      step_id <- as.character(step_id)
      filter_id <- as.character(filter_id)

      private$steps[[step_id]]$filters[[filter_id]] <- NULL
      if (length(private$steps[[step_id]]$filters) == 0) {
        self$remove_step(step_id, run_flow)
      } else {
        if (run_flow) {
          self$run_flow(min_step = step_id)
        }
      }
    },
    #' @description
    #' Update filter definition
    #' @param step_id Id of the step where filter is defined.
    #' @param filter_id Id of the filter to be updated.
    #' @param ... Filter parameters that should be updated.
    #' @param active Mark filter as active (`TRUE`) or inactive (`FALSE`).
    update_filter = function(step_id, filter_id, ..., active, run_flow = FALSE) {
      step_id <- as.character(step_id)
      filter_id <- as.character(filter_id)

      filter_env <- environment(private$steps[[step_id]]$filters[[filter_id]]$filter_data)
      new_args <- list(...)
      if (any(static_params %in% names(new_args))) {
        warning(glue::glue("Cannot modify filter {paste(sQuote(static_params), collapse = ', ')} parameters."))
      }

      params_to_change <- setdiff(names(new_args), static_params)
      any_changed <- FALSE # checking if any parameter changed to optimize calculations

      for (param_name in params_to_change) {
        new_val <- new_args[[param_name]]
        if (!identical(filter_env[[param_name]], new_val)) {
          any_changed <- TRUE
          filter_env[[param_name]] <- new_val
        }
      }

      if (!missing(active)) {
        filter_env[["active"]] <- active
      }

      if (run_flow && (!missing(active) || any_changed)) {
        self$run_flow(step_id)
      }
    },
    #' @description
    #' Reset filter to its default values.
    #' @param step_id Id of the step where filter is defined.
    #' @param filter_id Id of the filter which should be cleared.
    clear_filter = function(step_id, filter_id, run_flow = FALSE) {
      step_id <- as.character(step_id)
      filterd_id <- as.character(filter_id)

      do.call(
        self$update_filter,
        append(
          list(step_id = step_id, filter_id = filter_id, run_flow = run_flow),
          self$get_filter(step_id, filter_id)$get_defaults(
            self$get_data(step_id, collect = FALSE, state = "pre"),
            self$get_cache(step_id, filter_id, state = "pre")
          )
        )
      )
    },
    #' @description
    #' Reset all filters included in selected step.
    #' @param step_id Id of the step where filters should be cleared.
    clear_step = function(step_id, run_flow = FALSE) {
      step_id <- as.character(step_id)
      for (filter_id in names(self$get_step(step_id)$filters)) {
        self$clear_filter(step_id, filter_id, run_flow = FALSE)
      }
      if (run_flow) {
        self$run_flow(step_id)
      }
    },
    #' @description
    #' Sum up Cohort configuration - Source, steps definition and evaluated data.
    sum_up_state = function() {
      list(
        source = !is.null(private$source),
        source_call = attr(private$source, "call"),
        source_vars = names(private$source$attributes),
        n_steps = length(private$steps),
        steps_structure = step_filter_state(private$steps, method = names, raw = TRUE),
        n_filters = step_filter_state(private$steps),
        n_evaled_data = length(private$data_objects)
      )
    },
    #' @description
    #' Get Cohort configuration state.
    #' @param step_id If provided, the selected step state is returned.
    #' @param json If TRUE, return state in JSON format.
    #' @param extra_fields Names of extra fields included in filter to be added to state.
    get_state = function(step_id, json = FALSE, extra_fields = NULL) {

      if (missing(step_id)) {
        step_id <- names(private$steps)
      }

      get_filters_state <- function(filters) {
        filters %>% purrr::map(get_filter_state, extra_fields = extra_fields) %>% unname()
      }

      filters_state <- private$steps[step_id] %>%
        purrr::imap(~ list(step = .y, filters = get_filters_state(.x$filters))) %>%
        unname()

      if (json) {
        return(jsonlite::toJSON(filters_state, auto_unbox = TRUE, na = "string", null = "null"))
      }

      return(filters_state)
    },
    #' Restore Cohort configuration.
    #' @param state List or JSON string containing steps and filters configuration.
    #' @param modifier Function two parameters combining the previous and provided state.
    #'   The returned state is then restored.
    restore = function(state, modifier = function(prev_state, state) {state},
                       run_flow = FALSE, hook = list(
      pre = get_hook("pre_restore_hook"),
      post = get_hook("post_restore_hook")
    )) {

      self$attributes$pre_restore_state <- self$get_state(json = FALSE)

      run_hooks(hook$pre, self, private, state)

      if (is.null(state)) {
        return(invisible(FALSE))
      }

      if (is.character(state)) {
        state <- jsonlite::fromJSON(txt = state, simplifyVector = TRUE, simplifyMatrix = FALSE, simplifyDataFrame = FALSE)
      }

      state <- modifier(self$attributes$pre_restore_state, state)

      private$steps <- NULL
      private$cache <- NULL
      private$data_objects <- private$data_objects["0"]

      na_fix <- function(params) {
        params %>%
          purrr::modify_if(~ identical(., "NA"), ~ NA)
      }
      for (step_state in state) {
        for (filter_state in step_state$filters) {
          if (filter_state$type == "date_range") {
            filter_state$range <- as.Date(filter_state$range)
          }
          add_filter(
            self,
            do.call(filter, na_fix(filter_state)),
            step_id = step_state$step
          )
        }
      }
      if (run_flow) {
        self$run_flow()
      }
      run_hooks(hook$post, self, private, state)
    },
    #' @description
    #' Get step related data
    #' @param step_id Id of the step from which to source data.
    #' @param state Return data before ("pre") or after ("post") step filtering?
    #' @param collect Return raw data source (`FALSE`) object or collected (to R memory) data (`TRUE`).
    get_data = function(step_id, state = "post", collect = TRUE) {
      if (missing(step_id)) {
        step_id <- self$last_step_id()
      }
      data_id <- as.character(step_id)
      if (state == "pre") {
        data_id <- prev_step(step_id)
      }
      if (collect) {
        .collect_data(private$source, private$data_objects[[data_id]])
      } else {
        private$data_objects[[data_id]]
      }
    },
    #' @description
    #' Plot filter specific data summary.
    #' @param step_id Id of the step where filter is defined.
    #' @param filter_id Id of the filter for which the plot should be returned
    #' @param ... Another parameters passed to filter specific method.
    #' @param state Generate plot on data before ("pre") or after ("post") step filtering?
    plot_data = function(step_id, filter_id, ..., state = "post") {
      data_id <- as.character(step_id)
      if (state == "pre") {
        data_id <- prev_step(step_id)
      }
      private$steps[[step_id]]$filters[[filter_id]]$plot_data(
        private$data_objects[[data_id]],
        ...
      )
    },
    #' @description
    #' Show attrition plot.
    #' @param ... Source specific parameters required to generate attrition.
    #' @param percent Should attrition changes be presented with percentage values.
    show_attrition = function(..., percent = FALSE) {

      keep_active_state <- function(step_state) {
        step_state$filters <- step_state$filters %>%
          purrr::keep(~.$active)
        step_state
      }
      get_filter_meta <- function(filter_state) {
        filter_state$name <- filter_state$id
        filter_state$value_name <- filter_state$input_param
        filter_state$value <- filter_state[[filter_state$input_param]]

        return(filter_state)
      }
      active_states <- self$get_state(json = FALSE, extra_fields = "input_param") %>%
        purrr::map(keep_active_state)
      attrition_labels <- .get_attrition_label(
        source = self$get_source(),
        step_id = "0",
        step_filters = NULL,
        ...
      )
      for (active_state in active_states) {
        attrition_labels[length(attrition_labels) + 1] <- .get_attrition_label(
          source = self$get_source(),
          step_id = active_state$step,
          step_filters = purrr::map(active_state$filters, get_filter_meta),
          ...
        )
      }

      attrition_count <- .get_attrition_count(
        source = self$get_source(),
        data_stats = private$cache,
        ...
      )

      attrition_table <- get_attrition_coords(
        attrition_labels,
        attrition_count,
        percent = percent
      )

      get_attrition_plot(attrition_table)
    },
    #' @description
    #' Get Cohort related statistics.
    #' @param step_id When `filter_id` specified, `step_id` precises from which step the filter comes from.
    #'    Otherwise data from specified step is used to calculate required statistics.
    #' @param filter_id If not missing, filter related data statistics are returned.
    #' @param ... Specific parameters passed to filter related method.
    #' @param state Should the stats be calculated on data before ("pre") or after ("post")
    #'    filtering in specified step.
    get_stats = function(step_id, filter_id, ..., state = "post") {
      data_id <- as.character(step_id)
      if (state == "pre") {
        data_id <- prev_step(step_id)
      }
      if (missing(filter_id)) {
        return(
          .get_stats(private$source, private$data_objects[[data_id]])
        )
      }
      private$steps[[step_id]]$filters[[filter_id]]$get_stats(
        private$data_objects[[data_id]],
        ...
      )
    },
    #' @description
    #' Show source data or filter description
    #'
    #' @param field Name of the source description field provided as `description` argument to \link{set_source}.
    #'     If missing, `step_id` and `filter_id` are used to return filter description.
    #' @param filter_id Id of the filter to return description of.
    #' @param step_id Id of the filter step to return description of.
    #' @param modifier A function taking the description as argument.
    #'     The function can be used to modify its argument (convert to html, display in browser etc.).
    show_help = function(
      field, step_id, filter_id,
      modifier = getOption("cb_help_modifier", default = function(x) x)
    ) {
      description <- NULL
      if (!missing(field)) {
        if (is.null(self$get_source()$description)) return(NULL)
        description <- self$get_source()$description[[field]]
      }
      if (!missing(step_id) && !missing(filter_id)) {
        filter <- self$get_filter(step_id, filter_id)
        description <- filter$get_params("description")
      }
      return(modifier(description))
    },
    #' @description
    #' Return reproducible data filtering code.
    #' @param step_id If `step_id` specified and `filter_id` missing, reproducible
    #'     code for specified step is returned.
    #'     If `filter_id` not missing, `step_id` defines from which step the filter should be taken.
    #' @param filter_id If not missing, precises the filter for which reproducible code should be returned.
    #' @param ... Other parameters passed to \link[formatR]{tidy_source}.
    get_code = function(step_id, filter_id, ...) {
      source_expr <- if (!is.null(private$source$source_code)) {
        private$source$source_code
      } else {
        substitute(source <- list(dtconn = x), list(x = attr(private$source$dtconn, "call")))
      }
      source_type <- class(private$source)[1]

      init_step_expr <- parse_func_expr(
        .get_method(paste0(".init_step", ".", source_type))
      )
      run_binding_fun_expr <- quote({})
      bind_keys_expr <- quote({})
      pre_run_binding_expr <- quote({})
      run_binding_expr <- quote({})
      pre_filtering_expr <- post_filtering_expr <- quote({})
      pre_filtering_fun_expr <- method_to_expr(".pre_filtering", source_type)
      post_filtering_fun_expr <- method_to_expr(".post_filtering", source_type)
      if (length(pre_filtering_fun_expr) > 1) {
        pre_filtering_expr <- quote({
          data_object <- .pre_filtering(source, data_object, step_id)
        })
      }
      if (length(post_filtering_fun_expr) > 1) {
        post_filtering_expr <- quote({
          data_object <- .post_filtering(source, data_object, step_id)
        })
      }
      if (!is.null(private$source$binding_keys)) {
        run_binding_fun_expr <- method_to_expr(".run_binding", source_type)
        if (isTRUE(private$source$options$display_binding)) {
          bind_keys_expr <- assign_expr(quote(binding_keys), private$source$binding_keys)
        }
        pre_run_binding_expr <- quote({
          pre_data_object <- data_object
        })
        run_binding_expr <- quote({
          for (binding_key in binding_keys) {
            data_object <- .run_binding(
              source, binding_key,
              pre_data_object,
              data_object
            )
          }
        })
      }
      if (!missing(filter_id)) {
        exprs_list <- parse_filter_expr(private$steps[[step_id]]$filters[[filter_id]])
        return(combine_expressions(exprs_list))
      }
      get_step_code <- function(step_id, private) {
        filtering_expr <- private$steps[[step_id]]$filters %>%
          purrr::keep(~ .x$get_params("active")) %>%
          purrr::map(~ parse_filter_expr(.x))
        list(
          glue::glue("# step {step_id} "),
          substitute({step_id <- step_value}, list(step_value = step_id)),
          pre_filtering_expr,
          filtering_expr,
          post_filtering_expr,
          pre_run_binding_expr
        )
      }
      if (!missing(step_id)) {
        exprs_list <- get_step_code(step_id, private)
      } else {
        exprs_list <- names(self$get_step()) %>%
          purrr::map(get_step_code, private = private)
      }

      exprs_list <- append(
        append(
          list(
            bind_keys_expr, run_binding_fun_expr, pre_filtering_fun_expr, post_filtering_fun_expr,
            source_expr, init_step_expr
          ),
          exprs_list
        ),
        list(run_binding_expr, quote({data_object}))
      )

      # todo code include once
      res_quote <- combine_expressions(unlist(exprs_list))
      formatR::tidy_source(
        text = as.character(res_quote)[-1],
        ...
      )
    },
    #' @description
    #' Trigger data calculations sequentially.
    #' @param min_step Step id starting from the calculation will be started.
    run_flow = function(min_step,
                        hook = list(pre = get_hook("pre_run_flow_hook"), post = get_hook("post_run_flow_hook"))) {
      run_hooks(hook$pre, self, private)
      if (missing(min_step)) {
        min_step <- 1
      }
      min_step <- min(length(private$data_objects), as.integer(min_step)) # make sure all steps data is evaluated
      steps_to_execute <- steps_range(min_step, length(private$steps))
      for (data_idx in steps_to_execute) {
        self$run_step(data_idx)
      }
      run_hooks(hook$post, self, private)
    },
    #' @description
    #' Trigger data calculations for selected step.
    #' @param step_id Id of the step for which to run data calculation.
    run_step = function(step_id,
                        hook = list(
                          pre = get_hook("pre_run_step_hook"),
                          post = get_hook("post_run_step_hook")
                        )) {
      step_id <- as.character(step_id)

      run_hooks(hook$pre, self, private, step_id)
      temp_data_object <- .pre_filtering(
        source = private$source,
        data_object = private$data_objects[[prev_step(step_id)]],
        step_id = step_id
      )
      for (data_filter in self$get_filter(step_id)) {
        if (data_filter$get_params("active")) {
          temp_data_object <- temp_data_object %>%
            data_filter$filter_data()
        }
      }

      private$data_objects[[step_id]] <- .post_filtering(
        source = private$source,
        data_object = temp_data_object,
        step_id = step_id
      )
      self$bind_data(step_id)

      filter_ids <- names(self$get_step(step_id)$filters)
      is_cached <- !is.null(self$get_cache(step_id, state = "pre"))

      # todo make sure is_cached logic is correct
      if (!is_cached) {
        self$update_cache(step_id, state = "pre")
      }
      self$update_cache(step_id, state = "post")
      for (filter_id in filter_ids) {
        is_cached <- !is.null(self$get_cache(step_id, filter_id, state = "pre"))
        if (!is_cached) {
          self$update_cache(step_id, filter_id, state = "pre")
        }
        self$update_cache(step_id, filter_id, state = "post")
      }

      run_hooks(hook$post, self, private, step_id)
    },
    #' @description
    #' Run data binding for selected step.
    #'   See more at \link{binding-keys}.
    #' @param step_id Id of the step for which to bind the data.
    bind_data = function(step_id) {
      binding_keys <- private$source$binding_keys

      if (is.null(binding_keys)) {
        return(FALSE)
      }

      for (binding_key in binding_keys) {
        private$data_objects[[step_id]] <- .run_binding(
          private$source, binding_key,
          self$get_data(step_id, state = "pre", FALSE),
          self$get_data(step_id, state = "post", FALSE)
        )
      }

      return(TRUE)
    },
    #' @description
    #' Print defined steps configuration.
    describe_state = function() {
      if (length(private$steps) == 0) {
        cat("No steps configuration found.")
      } else {
        private$steps %>% purrr::walk(print_step)
      }
    },
    #' @description
    #' Get selected step configuration.
    #' @param step_id Id of the step to be returned.
    get_step = function(step_id) {
      if (!missing(step_id)) {
        private$steps[[as.character(step_id)]]
      } else {
        private$steps
      }
    },
    #' @description
    #' Get selected filter configuration.
    #' @param step_id Id of the step where filter is defined.
    #' @param filter_id If of the filter to be returned.
    #' @param method Custom function taking filters list as argument.
    get_filter = function(step_id, filter_id, method = function(x) x) {
      if (!missing(filter_id)) {
        method(private$steps[[as.character(step_id)]]$filters[[filter_id]])
      } else {
        method(private$steps[[as.character(step_id)]]$filters)
      }
    },
    #' @description
    #' Update filter or step cache.
    #' Caching is saving step and filter attached data statistics such as number of
    #' data rows, filter choices or frequencies.
    #' @param step_id Id of the step for which caching should be applied.
    #'   If `filter_id` is not missing, the parameter describes id of the step where filter should be found.
    #' @param filter_id Id of the filter for which caching should be applied.
    #' @param state Should caching be done on data before ("pre") or after ("post")
    #'    filtering in specified step.
    update_cache = function(step_id, filter_id, state = "post") {
      cache_id <- step_id
      if (state == "pre") {
        cache_id <- prev_step(step_id)
      }
      if (missing(filter_id)) {
        prev_cache <- private$cache[[cache_id]]
        cache_changed <- FALSE
        private$cache[[cache_id]] <- .get_stats(private$source, self$get_data(step_id, state, FALSE))
        if (!identical(prev_cache, private$cache[[cache_id]])) {
          cache_changed <- TRUE
        }
        private$cache[[cache_id]]$changed <- cache_changed
      } else {
        filter <- self$get_filter(step_id, filter_id)
        prev_cache <- private$cache[[cache_id]]$filters[[filter_id]]
        cache_changes <- FALSE
        private$cache[[cache_id]]$filters[[filter_id]] <- filter$get_stats(self$get_data(step_id, state, FALSE))
        if (!identical(prev_cache, private$cache[[cache_id]]$filters[[filter_id]])) {
          cache_changed <- TRUE
        }
        private$cache[[cache_id]]$filters[[filter_id]]$changed <- cache_changed
      }
    },
    #' @description
    #' Return step of filter specific cache.
    #' @param step_id Id of the step for which cached data should be returned
    #'   If `filter_id` is not missing, the parameter describes id of the step where filter should be found.
    #' @param filter_id Id of the filter for which cache data should be returned.
    #' @param state Should cache be returned on data before ("pre") or after ("post")
    #'    filtering in specified step.
    get_cache = function(step_id, filter_id, state = "post") {
      step_id <- as.character(step_id)
      if (state == "pre") {
        step_id <- prev_step(step_id)
      }
      if (missing(filter_id)) {
        private$cache[[step_id]]
      } else {
        private$cache[[step_id]]$filters[[filter_id]]
      }
    },
    #' @description
    #' List active filters included in selected step.
    #' @param step_id Id of the step where filters should be found.
    list_active_filters = function(step_id) {
      get_active_filters <- function(step_id, self) {
        active_names <- self$get_filter(step_id) %>%
          purrr::keep(~ .x$get_params("active")) %>%
          names()
        active_names
      }

      if (missing(step_id)) {
        names(self$get_step()) %>%
          purrr::map(get_active_filters, self = self) %>%
          unlist()
      } else {
        step_id <- as.character(step_id)
        get_active_filters(step_id, self)
      }
    },
    #' @description
    #' Return id of the last existing step in Cohort.
    last_step_id = function() {
      as.character(length(private$steps))
    },
    #' @description
    #' Helper method enabling to run non-standard operation on Cohort object.
    #' @param modifier Function of two arguments `self` and `private`.
    modify = function(modifier) {
      modifier(self, private)
    },
    #' @field attributes List of Cohort attributes defined while creating a new Cohort object.
    attributes = list()
  ),
  private = list(
    source = NULL,
    steps = list(),
    cache = list(),
    data_objects = list(),
    init_source = function(source, ...) {
      private$source <- source
      private$steps <- register_steps_and_filters(source, ...)
      initial_data <- .init_step(source)
      if (!is.null(initial_data)) {
        # important note: data objects are indexed from 0, whereas steps and filters from 1
        private$data_objects[["0"]] <- initial_data
      }
    }
  )
)

#' Create new 'Cohort' object
#'
#' Cohort object is designed to make operations on source data possible.
#' @param source Source object created with \link{set_source}.
#' @param run_flow If `TRUE`, data flow is run after the operation is completed.
#' @param hook List of hooks describing methods before/after the Cohort is created.
#'     See \link{hooks} for more details.
#' @param ... Steps definition (optional). Can be also defined as a sequence of
#'     filters - the filters will be added to the first step.
#' @return The object of class `Cohort`.
#'
#' @name create-cohort
#' @export
cohort <- function(source, ..., run_flow = FALSE,
                   hook = list(
                     pre = get_hook("pre_cohort_hook"),
                     post = get_hook("post_cohort_hook")
                   )) {
  Cohort$new(source, ..., run_flow = run_flow, hook = hook)
}

#' @title Managing the Cohort object
#'
#' @description
#' The list of methods designed for managing the Cohort configuration and state.
#'
#' \itemize{
#'    \item{\link{add_source}}{ Add source to Cohort object.}
#'    \item{\link{update_source}}{ Update Cohort object source.}
#'    \item{\link{add_step}}{ Add step to Cohort object.}
#'    \item{\link{rm_step}}{ Remove step from Cohort object.}
#'    \item{\link{add_filter}}{ Add filter to Cohort step.}
#'    \item{\link{rm_filter}}{ Remove filter from Cohort step.}
#'    \item{\link{update_filter}}{ Update filter configuration.}
#'    \item{\link{run}}{ Run data filtering.}
#' }
#'
#' @return The object of class `Cohort` having the modified configuration dependent on the used method.
#' @name managing-cohort
NULL

#' Add source to Cohort object.
#'
#' When Cohort object has been created without source, the method allows to
#' attach it.
#'
#' @param x Cohort object.
#' @param source Source object to be attached.
#' @return The `Cohort` class object with `Source` attached to it.
#'
#' @seealso \link{managing-cohort}
#' @export
add_source <- function(x, source) {
  x$add_source(source)
  return(invisible(x))
}

#' Update source in Cohort object.
#'
#' @param x Cohort object.
#' @param source Source object to be updated in Cohort.
#' @param keep_steps If `TRUE`, steps definition remain unchanged when updating source.
#'    If `FALSE` steps configuration is deleted.
#'    If vector of type integer, specified steps will remain.
#' @param run_flow If `TRUE`, data flow is run after the source is updated.
#' @return The `Cohort` class object with updated `Source` definition.
#'
#' @seealso \link{managing-cohort}
#' @export
update_source <- function(x, source, keep_steps = !has_steps(source), run_flow = FALSE) {
  x$update_source(source, keep_steps, run_flow)
  return(invisible(x))
}

#' Add filtering step definition
#'
#' @param x An object to add step to.
#' @param step Step definition created with \link{step}.
#' @param ... Other parameters passed to specific S3 method.
#' @return Method dependent object (i.e. `Cohort` or `Source`) having new step added.
#'
#' @seealso \link{managing-cohort}, \link{managing-source}
#' @export
add_step <- function(x, step, ...) {
  UseMethod("add_step", x)
}

#' @rdname add_step
#' @param run_flow If `TRUE`, data flow is run after the step is added.
#' @param hook List of hooks describing methods to run before/after the step is added.
#'     See \link{hooks} for more details.
#' @export
add_step.Cohort <- function(x, step, run_flow = FALSE,
                            hook = list(
                              pre = get_hook("pre_add_step_hook"),
                              post = get_hook("post_add_step_hook")
                            ), ...) {
  x$add_step(step, run_flow = run_flow, hook = hook)
  return(invisible(x))
}

#' Remove filtering step definition
#'
#' @param x An object from which step should be removed.
#' @param step_id Id of the step to remove.
#' @param ... Other parameters passed to specific S3 method.
#' @return Method dependent object (i.e. `Cohort` or `Source`) having selected step removed.
#'
#' @seealso \link{managing-cohort}, \link{managing-source}
#' @export
rm_step <- function(x, step_id, ...) {
  UseMethod("rm_step", x)
}

#' @rdname rm_step
#' @param run_flow If `TRUE`, data flow is run after the step is removed.
#' @param hook List of hooks describing methods before/after the Cohort is created.
#'     See \link{hooks} for more details.
#' @export
rm_step.Cohort <- function(x, step_id, run_flow = FALSE,
                           hook = list(
                             pre = get_hook("pre_rm_step_hook"),
                             post = get_hook("post_rm_step_hook")
                           ), ...) {
  x$remove_step(step_id, run_flow = run_flow, hook = hook)
  return(invisible(x))
}

#' Add filter definition
#'
#' @param x An object to add filter to.
#' @param filter Filter definition created with \link{filter}.
#' @param step_id Id of the step to add the filter to.
#'     If missing, filter is added to the last step.
#' @param ... Other parameters passed to specific S3 method.
#' @return Method dependent object (i.e. `Cohort` or `Source`) having filter added in selected step.
#'
#' @seealso \link{managing-cohort}, \link{managing-source}
#' @export
add_filter <- function(x, filter, step_id, ...) {
  UseMethod("add_filter", x)
}

#' @rdname add_filter
#' @param run_flow If `TRUE`, data flow is run after the filter is added.
#' @export
add_filter.Cohort <- function(x, filter, step_id, run_flow = FALSE, ...) {
  x$add_filter(filter, step_id, run_flow)
  return(invisible(x))
}

#' Remove filter definition
#'
#' @param x An object from which filter should be removed.
#' @param step_id Id of the step from which filter should be removed.
#' @param filter_id Id of the filter to be removed.
#' @param ... Other parameters passed to specific S3 method.
#' @return Method dependent object (i.e. `Cohort` or `Source`) having selected filter removed.
#'
#' @seealso \link{managing-cohort}, \link{managing-source}
#' @export
rm_filter <- function(x, step_id, filter_id, ...) {
  UseMethod("rm_filter", x)
}

#' @rdname rm_filter
#' @param run_flow If `TRUE`, data flow is run after the filter is removed.
#' @export
rm_filter.Cohort <- function(x, step_id, filter_id, run_flow = FALSE, ...) {
  x$remove_filter(step_id, filter_id, run_flow)
  return(invisible(x))
}

#' Update filter definition
#'
#' @param x An object in which the filter should be updated.
#' @param step_id Id of the step where filter is defined.
#' @param filter_id Id of the filter to be updated.
#' @param ... Filter parameters that should be updated.
#' @return Method dependent object (i.e. `Cohort` or `Source`) having selected filter updated.
#'
#' @seealso \link{managing-cohort}, \link{managing-source}
#' @export
update_filter <- function(x, step_id, filter_id, ...) {
  UseMethod("update_filter", x)
}

#' @rdname update_filter
#' @param run_flow If `TRUE`, data flow is run after the filter is updated.
#' @export
update_filter.Cohort <- function(x, step_id, filter_id, ..., run_flow = FALSE) {
  x$update_filter(step_id, filter_id, ..., run_flow = run_flow)
  return(invisible(x))
}

#' Trigger data calculations.
#'
#' @param x Cohort object.
#' @param min_step_id Step id starting from the calculation will be started.
#'     Used only when `step_id` is missing.
#' @param step_id Id of the step for which to run data calculation.
#' @return The object of class `Cohort` having up-to-date data based on the Cohort state.
#'
#' @seealso \link{managing-cohort}
#' @export
run <- function(x, min_step_id, step_id) {
  if (!missing(step_id)) {
    x$run_step(step_id)
    return(invisible(x))
  }
  x$run_flow(min_step_id)
  return(invisible(x))
}

#' Cohort related methods
#'
#' @description
#' The list of methods designed for getting Cohort-related details.
#'
#' \itemize{
#'    \item{\link{plot_data}}{ Plot filter related Cohort data.}
#'    \item{\link{stat}}{ Get Cohort related statistics.}
#'    \item{\link{code}}{ Return reproducible data filtering code.}
#'    \item{\link{get_data}}{ Get step related data.}
#'    \item{\link{sum_up}}{ Sum up Cohort state.}
#'    \item{\link{get_state}}{ Save Cohort state.}
#'    \item{\link{restore}}{ Restore Cohort state.}
#'    \item{\link{attrition}}{ Show attrition plot.}
#'    \item{\link{description}}{ Show Source or filter related description.}
#' }
#'
#' @return Various type outputs dependent on the selected method.
#'   See each method documentation for details.
#' @name cohort-methods
NULL

#' Plot filter related Cohort data.
#'
#' For specified filter the method calls filter-related plot method to present data.
#'
#' @param x Cohort object.
#' @param step_id Id of step in which the filter was defined..
#' @param filter_id Filter id.
#' @param ... Another parameters passed to filter plotting method.
#' @param state Generate plot based on data before ("pre") or after ("post") filtering.
#' @return Filter-specific plot.
#'
#' @seealso \link{cohort-methods}
#' @export
plot_data <- function(x, step_id, filter_id, ..., state = "post") {
  x$plot_data(step_id, filter_id, ..., state = state)
}

#' Get Cohort related statistics.
#'
#' Display data statistics related to specified step or filter.
#'
#' @param x Cohort object.
#' @param step_id When `filter_id` specified, `step_id` precises from which step the filter comes from.
#'    Otherwise data from specified step is used to calculate required statistics.
#' @param filter_id If not missing, filter related data statistics are returned.
#' @param ... Specific parameters passed to filter related method.
#' @param state Should the stats be calculated on data before ("pre") or after ("post")
#'    filtering in specified step.
#' @return List of filter-specific values summing up underlying filter data.
#'
#' @seealso \link{cohort-methods}
#' @export
stat <- function(x, step_id, filter_id, ..., state = "post") {
  x$get_stats(step_id, filter_id, ..., state = state)
}

#' Return reproducible data filtering code.
#'
#' @param x Cohort object.
#' @param step_id If `step_id` specified and `filter_id` missing, reproducible
#'     code for specified step is returned.
#'     If `filter_id` not missing, `step_id` defines from which step the filter should be taken.
#' @param filter_id If not missing, precises the filter for which reproducible code should be returned.
#' @param ... Other parameters passed to \link[formatR]{tidy_source}.
#' @return \link[formatR]{tidy_source} output storing reproducible code for generating final step data.
#'
#' @seealso \link{cohort-methods}
#' @export
code <- function(x, step_id, filter_id, ...) {
  x$get_code(step_id, filter_id, ...)
}

#' Get step related data
#'
#' @param x Cohort object.
#' @param step_id Id of the step from which to source data.
#' @param state Return data before ("pre") or after ("post") step filtering?
#' @param collect Return raw data source (`FALSE`) object or collected (to R memory) data (`TRUE`).
#' @return Subset of Source-specific data connection object or its evaluated version.
#'
#' @seealso \link{cohort-methods}
#' @export
get_data <- function(x, step_id, state = "post", collect = FALSE) {
  x$get_data(step_id, state = state, collect = collect)
}

#' Sum up Cohort state.
#'
#' @param x Cohort object.
#' @return None (invisible NULL). Printed summary of Cohort state.
#'
#' @seealso \link{cohort-methods}
#' @export
sum_up <- function(x) {
  x$describe_state()
}

#' Get Cohort configuration state.
#'
#' @param x Cohort object.
#' @param step_id If provided, the selected step state is returned.
#' @param json If TRUE, return state in JSON format.
#' @param extra_fields Names of extra fields included in filter to be added to state.
#' @return List object of character string being the list convertion to JSON format.
#'
#' @seealso \link{cohort-methods}
#' @export
get_state <- function(x, step_id, json = FALSE, extra_fields = NULL) {
  x$get_state(step_id = step_id, json = json, extra_fields = extra_fields)
}
#' Restore Cohort object.
#'
#' The method allows to restore Cohort object with provided configuration state.
#'
#' @param x Cohort object.
#' @param state List or JSON string containing steps and filters configuration.
#'   See \link{get_state}.
#' @param modifier Function two parameters combining the previous and provided state.
#'   The returned state is then restored.
#' @param run_flow If TRUE, filtering flow is applied when the operation is finished.
#' @return The `Cohort` class object having the state restored based on provided config.
#'
#' @seealso \link{cohort-methods}
#' @export
restore <- function(x, state, modifier = function(prev_state, state) state, run_flow = FALSE) {
  x$restore(state = state, modifier = modifier, run_flow = run_flow)
  return(invisible(x))
}

#' Show attrition plot.
#'
#' @param x Cohort object.
#' @param ... Source specific parameters required to generate attrition.
#' @param percent Should attrition changes be presented with percentage values.
#' @return Plot object of class `ggplot`.
#'
#' @seealso \link{cohort-methods}
#' @export
attrition <- function(x, ..., percent = FALSE) {
  x$show_attrition(..., percent = percent)
}

#' Show source data or filter description
#'
#' If defined allows to check the provided description related to source data or configured filters.
#'
#' @param x Cohort object.
#' @param field Name of the source description field provided as `description` argument to \link{set_source}.
#'     If missing, `step_id` and `filter_id` are used to return filter description.
#' @param filter_id Id of the filter to return description of.
#' @param step_id Id of the filter step to return description of.
#' @param modifier A function taking the description as argument.
#'     The function can be used to modify its argument (convert to html, display in browser etc.).
#' @return Any object (or its subset) attached to Source of filter via description argument.
#'
#' @seealso \link{cohort-methods}
#' @export
description <- function(x, field, step_id, filter_id,
                      modifier = getOption("cb_help_modifier", default = function(x) x)) {
  x$show_help(field = field, step_id = step_id, filter_id = filter_id, modifier = modifier)
}
