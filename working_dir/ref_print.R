library(simmer)

t0 <- create_trajectory("my trajectory") %>%
  seize("server", 1) %>%
  timeout(function() rexp(1, 1)) %>%
  branch(function() sample(1:2, 1), c(F, T),
         create_trajectory("branch1") %>%
           seize("server", 2) %>%
           timeout(function() 1) %>%
           release("server", 2),
         create_trajectory("branch2") %>%
           seize("server", 4) %>%
           timeout(function() rexp(1, 3)) %>%
           release("server", 4)
  ) %>%
  rollback(1) %>%
  timeout(function() 1) %>%
  release("server", 1)

t0

a <- t0 %>% get_head()
a %>% print_activity(); a <- a %>% get_next_activity()
