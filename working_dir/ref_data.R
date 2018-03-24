library(simmer)

t0 <- trajectory() %>%
  log_(function() paste(
    "attr1:", get_attribute(env, "attr1"),
    "attr2:", get_attribute(env, "attr2"),
    "prior:", paste(get_prioritization(env), collapse=" ")
  ))

data <- data.frame(time=rep(1, 3), priority=1:3, attr1=11:13, attr2=21:23)

env <- simmer() $
  add_data("dummy", t0, data)

run(env)
