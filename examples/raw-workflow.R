# Operating on cohort object
library(cohortBuilder)
coh <- cohort(
  source = set_source(
    tblist(
      iris = iris, mtcars = mtcars,
      set = data.frame(name = c("setosa", "virginica"))
    ),
    binding_keys = bind_keys(
      bind_key(data_key("set", "name"), data_key("iris", "Species"))
    )
  )
) %>%
  add_filter(
    filter("discrete", id = "species", dataset = "iris", variable = "Species", value = c("setosa", "versicolor"))
  ) %>%
  add_filter(
    filter("range", dataset = "iris", variable = "Petal.Length", range = c(5, 6))
  ) %>%
  add_filter(
    filter("discrete", dataset = "set", variable = "name", value = "setosa")
  )

coh$sum_up_state()
sum_up(coh)

coh <- coh %>% run()

coh %>% code(include_methods = NULL)

get_data(coh, 1, state = "post")

coh <- coh %>%
  update_filter(1, 2, range = c(1, 1.5), run_flow = TRUE)

summary(coh)
get_data(coh, 1, state = "post")

coh <- coh %>% add_step(
  step(
    filter("range", dataset = "mtcars", variable = "qsec", range = c(14, 15))
  )
) %>%
  run(2)
get_data(coh, 2, state = "post")

stat(coh, 1, 2)
stat(coh, 1, 2, state = "post")

stat(coh, 1, 1)
stat(coh, 1, 1, state = "post")

stat(coh, 2, 1)
stat(coh, 2, 1, state = "post")

plot(coh, 1, 1, state = "pre")
plot(coh, 1, 1, state = "post")

plot(coh, 1, 2, state = "pre")
plot(coh, 1, 2, state = "post")

plot(coh, 2, 1, state = "pre")
plot(coh, 2, 1, state = "post")

coh %>% code()
coh %>% code(1, 1)
coh %>% code(2, 1)

coh$sum_up_state()

coh <- coh %>% rm_step(2, run_flow = TRUE)
coh$sum_up_state()

coh <- coh %>%
  update_source(
    set_source("raw", datasets = list(iris = iris)),
    keep_steps = TRUE
  )

coh <- coh %>% rm_filter(1, 2)
coh$sum_up_state()

coh <- coh %>% run()
get_data(coh, 1, state = "post")

# Operating on source object

raw_source <- set_source("raw", datasets = list(iris = iris, mtcars = mtcars)) %>%
  add_filter(
    filter("discrete", dataset = "iris", variable = "Species", value = c("setosa", "versicolor"))
  ) %>%
  add_filter(
    filter("range", dataset = "iris", variable = "Petal.Length", range = c(5, 6))
  )

coh <- raw_source %>%
  cohort() %>%
  run()

get_data(coh, 1, state = "post")

updated_source <- raw_source %>%
  update_filter(1, 2, range = c(1, 1.5))

coh <- updated_source %>%
  cohort() %>%
  run()

coh$sum_up_state()
get_data(coh, 1, state = "post")

extended_source <- updated_source %>% add_step(
  step(
    filter("range", dataset = "mtcars", variable = "qsec", range = c(14, 15))
  )
)

coh <- coh %>%
  update_source(extended_source) %>%
  run()
coh$sum_up_state()
get_data(coh, 2, state = "post")

stat(coh, 1, 2)
stat(coh, 1, 2, state = "post")

stat(coh, 1, 1)
stat(coh, 1, 1, state = "post")

stat(coh, 2, 1)
stat(coh, 2, 1, state = "post")

plot(coh, 1, 1, state = "pre")
plot(coh, 1, 1, state = "post")

plot(coh, 1, 2, state = "pre")
plot(coh, 1, 2, state = "post")

plot(coh, 2, 1, state = "pre")
plot(coh, 2, 1, state = "post")

coh %>% code(1)
coh %>% code(1, 1)
coh %>% code(2, 1)

coh$sum_up_state()

no_step_two_source <- extended_source %>%
  rm_step(2) %>%
  rm_filter(1, 2)
coh <- coh %>%
  update_source(no_step_two_source) %>%
  run()
coh$sum_up_state()

coh %>% get_data(1, state = "post")

# restore state

state <- '[{"step":"1","filters":[{"type":"discrete","id":"LYPUL1640690490344","name":"LYPUL1640690490344","variable":"Species","value":["setosa","versicolor"],"dataset":"iris","keep_na":true,"active":true},{"type":"range","id":"KWLWJ1640690490345","name":"KWLWJ1640690490345","variable":"Petal.Length","range":[5,6],"dataset":"iris","keep_na":true,"active":true}]}]'
coh <- cohort(
  source = set_source("raw", datasets = list(iris = iris, mtcars = mtcars))
)

coh$restore(state)
coh$sum_up_state()
coh %>% run()



