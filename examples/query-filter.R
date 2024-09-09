library(queryBuilder)
library(cohortBuilder)
library(magrittr)

my_query <- queryGroup(
  condition = "AND",
  queryRule("Sepal.Length", "greater", 5),
  queryGroup(
    condition = "OR",
    queryRule(
      field = "Petal.Length",
      operator = "greater",
      value = 3
    ),
    queryRule(
      field = "Petal.Width",
      operator = "less",
      value = 1
    )
  )
)
queryToExpr(my_query)

coh <- cohort(
  source = set_source(
    tblist(
      iris = iris, mtcars = mtcars
    )
  )
) %>%
  add_filter(
    filter("discrete", id = "species", dataset = "iris", variable = "Species", value = c("setosa", "versicolor"))
  ) %>%
  add_filter(
    filter(
      "query", id = "gen_query", dataset = "iris", variables = c("Sepal.Length", "Petal.Length", "Petal.Width"),
      value = my_query, keep_na = FALSE
    )
  )

coh$sum_up_state()
sum_up(coh)

coh <- coh %>% run()

coh %>% code(include_methods = NULL)

get_data(coh, 1, state = "post")

