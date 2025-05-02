pkgload::load_all()
describe <- function(description, ...) {
  # already_described <- is.list(description) && !is.null(description$text)
  # if (already_described) {
  #   return(already_description)
  # }
  list(
    oko = description,
    ...
  )
}

dt_source <- set_source(
  tblist(
    iris = iris,
    mtcars = mtcars
  ),
  description = list(
    iris = list(
      dataset_ = describe("Iris"),
      Petal.Length = describe("Petal.Length"),
      Species = describe("Species")
    ),
    mtcars = list(
      dataset_ = describe("MTcars"),
      qsec = describe("Qsec")
    )
  )
)

coh <- cohort(
  source = dt_source
) %>%
  add_filter(
    filter("discrete", id = "species", dataset = "iris", variable = "Species", value = c("setosa", "versicolor"))
  ) %>%
  add_filter(
    filter("range", dataset = "iris", variable = "Petal.Length", range = c(5, 6))
  ) %>%
  add_filter(
    filter("range", dataset = "mtcars", variable = "qsec", range = NA)
  )

shape(dt_source)


# data_source <- set_source(
#   tblist(),
#   available_filters = filters_config(
#     filter("deiscrete", "age", description = "Age filter")
#   ),
#   description = list(
#     datasets = list(
#       patients = list(
#         dataset_ = describe("patietns data", meta = "", embeeddings = function(name) take_from_db(name), "..."),
#         age = describe("Age filter", meta = "", embeeddings = function(name) take_from_db(name), "...")
#       )
#     )
#   )
# )



# Tool description:
# ellmer's tools (is it possible to keep cohort as argument)
set_step_tool <- function(cohort, action = c("edit_last", "new_step")) {
  action <- match.arg(action, several.ok = TRUE)
  fun <- function(filters, action = action) {
    action <- match.arg(action, several.ok = FALSE)
    check_ids(filters)
    make_sure_na_value(filters) # only for now, until we have values setting
    filters <- only_existing(filters)
    if (action == "edit_last") {
      cohort$edit_step(filters, step_id = last_step())
    }
    if (action == "new_step") {
      cohort$add_step(filters, step_id = last_step())
    }
    msg <- "Five filters were configured: age, gender, .... Rejected: idd, dnjsaklndkasl (this one was stupidly passed by llm)"
    return(msg)
  }
  attr(fun, "description") <- "dmkasldmksa"
  attr(fun, "params") <- list(...)
  return(fun)
}

# Tool description: when filter description is missing, use this tool to get filter description:
get_description_tool <- function(cohort, calculate_embeddings, top_k) {

  fun <- function(user_prompt = NULL) {
    filters_we_want <- cohort$get_filters() # all
    embeding_user_prompt <- calculate_embedings(user_prompt) # optim
    if (has_empeddings(cohort)) {
      filters_we_want <- sort(calc_dist(embedings, embeddings(cohort)))[1:top_k]
    }
    cohort$describe(filters = filters_we_want)
  }
}

# Tool description:
get_vocab_tool <- function(cohort, top_k) {
  fun <- function(filter_id) {
    if (filter_allows(filter_id)) {
      msg <- cohort$get_filters(filter_id, step_id = last_step())$get_stats("choices/range/values")
      msg <- calc_freq(msg, sort = TRUE)[1:top_k]
      return(msg)
    }
    return("restricted")
  }
}

set_filter_tool <- function(cohort) {
  fun <- function(filter_id, values) {
    if (not_in_meta(filter_id)) {
      return("Not existing filter_id")
    }
    if (!value %in% vocab(filter_id)) {
      return("Incorrect value")
    }
    if (filter_not_in_step(filter_id)) {
      filter <- get_from_meta(filter_id)
      filter <- set_value(filter, values)
      cohort$edit_step(filter, step_id = last_step())
    } else {
      msg <- cohort$update_filter(filter_id, step_id = last_step())
    }
    return("Udpdated")
  }
}

cohortBuilder$predefined_prompts <- list(
  "system_prompt" = "njdslnfjkdslnfkd",
  "use_tool_prompt" = "dnsajklsa"
)

cohortBuilder::set_tool <- function(chat, cb_tool, description, ...) {
  if (missing(description)) {
    description <- attr(cb_tool, "description")
  }
  args <- list(...)
  if (empty(args)) {
    args <- attr(cb_tool, "params")
  }
  chat$register_tool(tool(
    cb_tool(cohort, action = c("edit_last", "new_step")),
    description = description,
    !!!args
  ))
  return(chat)
}

chat <- chat_openai(model = "gpt-4o", system_prompt = "Please set demographics filters.")

chat |>
  set_tool(set_step_tool(cohort, action = c("edit_last")), decription = "dmskldmlksa") |>
  set_tool(get_description_tool(cohort))

chat$chat()


