library(simmer)

t1 <- create_trajectory("my trajectory", verbose=T) %>%
  ## add an intake activity
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1)

t2 <- create_trajectory(verbose=T) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1)

t3 <- create_trajectory(verbose=T) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

t0 <- join(t1, t2, t3)
t0 %>% get_head; t1 %>% get_head
t0 %>% get_tail; t3 %>% get_tail

t0 <- create_trajectory() %>%
  join(t1) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  join(t3)

t1 <- create_trajectory() %>%
  branch(function() 1, T,
         create_trajectory() %>% timeout(1))
