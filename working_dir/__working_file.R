library(simmer)

t <- trajectory(verbose = TRUE) %>%
  seize("asdf") %>%
  timeout(1) %>%
  wait() %>%
  timeout(2) %>%
  release("asdf")

t

join(t)

head(t, 2)
tail(t, 2)
t[c(5, 3, 1)]
t[-c(5, 3, 1)]
t["timeout"]
t[["timeout"]]
t[[3]]
