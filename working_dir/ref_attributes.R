library(simmer)

t0 <- trajectory() %>%
  set_attribute(
    c("att1", "att2"),
    c(1, 2)
  ) %>%
  timeout(1) %>%
  set_global(
    function() c("glb1", "glb2"),
    c(1, 2)
  ) %>%
  timeout(1) %>%
  set_attribute(
    c("att3", "att4"),
    function() get_attribute(env, c("att1", "att2")) + 2
  ) %>%
  timeout(1) %>%
  set_global(
    function() c("glb3", "glb4"),
    function() get_global(env, c("glb1", "glb2")) + 2
  )

env <- simmer() %>%
  add_generator("entity", t0, at(0), mon=2)
env %>%
  run() %>%
  get_mon_attributes()
