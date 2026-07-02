pkgload::load_all()

# -- Setup source and cohort ---------------------------------------------------

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
      mpg = describe("Miles/(US) gallon"),
      cyl = describe("Number of cylinders"),
      disp = describe("Displacement (cu.in.)"),
      hp = describe("Gross horsepower"),
      drat = describe("Rear axle ratio"),
      wt = describe("Weight (1000 lbs)"),
      qsec = describe("1/4 mile time"),
      vs = describe("Engine (0 = V-shaped, 1 = straight)"),
      am = describe("Transmission (0 = automatic, 1 = manual)"),
      gear = describe("Number of forward gears"),
      carb = describe("Number of carburetors")
    )
  )
) |> autofilter(attach_as = "meta")

shape(dt_source)

coh <- cohort(source = dt_source)

# -- Register tools with chat --------------------------------------------------

chat <- ellmer::chat_azure_openai(
  endpoint = Sys.getenv("CHAT_ENDPOINT"),
  model = "gpt-4o",
  api_version = "2024-08-01-preview",
  system_prompt = "You are a helpful assistant.",
  credentials = function() list("api-key" = Sys.getenv("CHAT_KEY"))
)

chat |> cb_register_tools(coh)

# -- Example usage -------------------------------------------------------------

chat$chat("Get information about available filters.")
chat$chat("What are the filters in mtcars dataset?")

# 1. Add filters in a new step without specifying values
chat$chat("Add species filter for iris and horsepower filter for cars.")
sum_up(coh)

# 2. Update existing filters with described values
chat$chat("Set species to versicolor only and horsepower between 100 and 200.")
sum_up(coh)

# 3. Add a new filter to the existing step
chat$chat("Also add the number of cylinders filter to the current step.")
sum_up(coh)

# 4. Add filters with specified values in a new step
chat$chat(
  "Filter iris species that start with 'v' letter and cars having horse power above 100.",
  "Make sure the filtering was applied in a new step."
)
sum_up(coh)

# -- Future ideas --------------------------------------------------------------

# Embedding-based tool: find relevant filters by semantic similarity
# get_description_tool <- function(cohort, calculate_embeddings, top_k) { ... }

# Vocabulary tool: get top-k frequent values for a filter
# get_vocab_tool <- function(cohort, top_k) { ... }
