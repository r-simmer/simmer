t0 <- Trajectory$new("my trajectory") $
  seize("server", 1) $
  timeout(function() rexp(1, 1)) $
  branch(prob=0.1, merge=F, Trajectory$new("branch1") $
    seize("server", 2) $
    timeout(function() 1) $
    release("server", 2)
  ) $
  branch(prob=0.9, merge=T, Trajectory$new("branch2") $
    seize("server", 4) $
    timeout(function() rexp(1, 3)) $
    release("server", 4)
  ) $
  timeout(function() 1) $
  release("server", 1)

a <- t0$get_head()
a$next_event
a <- a$next_event

t0$show()

