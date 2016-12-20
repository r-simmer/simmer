library(simmer)

t1 <- trajectory("my trajectory", verbose=T) %>%
  ## add an intake activity
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1)

t2 <- trajectory(verbose=T) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1)

t3 <- trajectory(verbose=T) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

t0 <- join(t1, t2, t3)
head(t0); head(t1)
tail(t0); tail(t3)

t0 <- trajectory() %>%
  join(t1) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  join(t3)

t1 <- trajectory() %>%
  branch(function() 1, T,
         trajectory() %>% timeout(1))
