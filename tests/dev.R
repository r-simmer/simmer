t0 <- Trajectory$new("my trajectory",
  SeizeEvent$new("server", 2),
  TimeoutEvent$new(function() rexp(1, 1)),
  ReleaseEvent$new("server", 2)
)

t0$show()

a <- Branch$new()
a$next_event