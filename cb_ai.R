pkgload::load_all()
describe <- function(description, ...) {
  # already_described <- is.list(description) && !is.null(description$text)
  # if (already_described) {
  #   return(already_description)
  # }
  list(
    text = description,
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
      dataset_ = describe("dataset related to iris plants"),
      Sepal.Length = describe("filter for the sepal length measurement"),
      Petal.Length = describe("filter for the petal length measurement"),
      Sepal.Width = describe("filter for the sepal width measurement"),
      Petal.Width = describe("filter for the petal width measurement"),
      Species = describe("filter for the species of iris")
    ),
    mtcars = list(
      dataset_ = describe("dataset related to car specifications"),
      mpg =	describe("Miles/(US) gallon"),
      cyl =	describe("Number of cylinders"),
      disp =	describe("Displacement (cu.in.)"),
      hp =	describe("Gross horsepower"),
      drat =	describe("Rear axle ratio"),
      wt =	describe("Weight (1000 lbs)"),
      qsec =	describe("1/4 mile time"),
      vs =	describe("Engine (0 = V-shaped, 1 = straight)"),
      am =	describe("Transmission (0 = automatic, 1 = manual)"),
      gear =	describe("Number of forward gears"),
      carb =	describe("Number of carburetors")
    )
  )
) |> shinyCohortBuilder::autofilter(attach_as = "meta")

dt_source

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

chat <- ellmer::chat_azure(
  endpoint = Sys.getenv("CHAT_ENDPOINT"),
  deployment_id = "gpt-4o",
  api_version = "2024-08-01-preview",
  system_prompt = "You are a helpful assistant.",
  credentials = list("api-key" = Sys.getenv("CHAT_KEY"))
)
#chat$chat("What is 1+1?")

set_chat_tool <- function(chat, cb_tool, description, ...) {
  if (missing(description)) {
    description <- attr(cb_tool, "description")
  }
  args <- list(...)
  if (length(args) == 0) {
    args <- attr(cb_tool, "params")
  }
  chat$register_tool(
    rlang::inject(
      ellmer::tool(
        .fun = cb_tool,
        .description = description,
        !!!args
      )
    )
  )
  return(chat)
}

get_filters_meta_tool <- function(cohort) {
  fun <- function() {
    filters_meta <- shape(cohort$get_source())
    return(jsonlite::toJSON(filters_meta, auto_unbox = TRUE))
  }
  attr(fun, "description") <- r"(
    The tool returns information of available filters in json format.
    The json is a set of objects, each object describing either dataset (when filter field is not specified) or filter description (otherwise).
    Fields named 'filter' are storing the filter id.
    Fields named 'dataset' are storing the dataset name that filter is attached to.
    Fields named 'desciption' are storing the description of filter purpose.
  )"
  attr(fun, "params") <- list(
    .name = "get_filters_meta"
  )
  return(fun)
}

set_chat_tool(chat, get_filters_meta_tool(coh))

#chat$chat("Get information about available filters.")
#chat$chat("What are the filters in mtcars dataset?")

sum_up(coh)

# Tool description:
# ellmer's tools (is it possible to keep cohort as argument)
set_step_filters_tool <- function(cohort, action = c("edit_last", "new_step"), ...) {
  action <- match.arg(action, several.ok = TRUE)
  fun <- function(filter_ids, action = action) {
    browser()
    filter_ids <- strsplit(filter_ids, ",")[[1]]
    action <- match.arg(action, several.ok = FALSE)
    available_filters <- cohort$attributes$available_filters
    filters_to_set <- available_filters %>%
      purrr::keep(function(x) {x$name %in% filter_ids})
    # if (action == "edit_last") {
    #   cohort$edit_step(filters, step_id = last_step())
    # }
    if (action == "new_step") {
      cohort$copy_step(
        filters = filters_to_set,
        run_flow = TRUE
      )
    }
    msg <- glue::glue("The following filters have been set: {paste(filter_ids, collapse = ', ')}")
    return(msg)
  }
  attr(fun, "description") <- r"(
    The tool used to set specific set of filters to a new filtering step.
    Available filters can be extracted using 'get_filters_meta' tool.
  )"
  attr(fun, "params") <- list(
    .name = "set_step_filters",
    filter_ids = ellmer::type_string(
      "JSON array storing filter ids that should be set to the cohort."
    ),
    action = ellmer::type_string(
      "Always equal to 'new_step' string."
    )
  )
  return(fun)
}

set_chat_tool(chat, set_step_filters_tool(coh))

sum_up(coh)

chat$chat("Set filters that will allow me to specify iris species and a car speed.")

sum_up(coh)

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





