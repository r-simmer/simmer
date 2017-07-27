library(simmer)

simmer() %>%
  add_generator("asdf", timeout(trajectory(), 1), function() rexp(100, 1)) %>%
  run(1e7)

gc() # still stored in .Last.value
gc() # freed

env <- simmer() %>%
  add_generator("asdf", timeout(trajectory(), 1), function() rexp(100, 1)) %>%
  run(1e7)

reset(env)

env <- simmer() %>%
  add_generator("asdf", timeout(trajectory(), 1), function() rexp(100, 1)) %>%
  run(1e7)

rm(env)
gc()
