library(simmer)

# define a 24-hour period
#  - from 8 to 16 h -> 10 units
#  - from 16 to 24 h -> 5 units
#  - from 24 to 8 h -> 1 unit
my_schedule <- schedule(timetable = c(8, 16, 24),
                        period = 24,
                        values = c(10, 5, 1))
my_schedule$get_schedule()

t0 <- create_trajectory() %>%
  seize("doctor", 1) %>%
  timeout(function() rexp(1, 1)) %>%
  release("doctor", 1)

env <- simmer() %>%
  add_resource("doctor", capacity=my_schedule) %>%
  add_generator("patient", t0, function() rexp(1, 5)) %>%
  run(200)

plot_resource_usage(env, "doctor")

t0<-
  create_trajectory() %>%
  seize("t-rex") %>%
  timeout(5) %>%
  release("t-rex")

sim<-
  simmer() %>%
  add_resource("t-rex", capacity = schedule(timetable = c(5,10,15), values = c(1,0,1), period=Inf)) %>%
  add_generator("piggy", t0, at(0,0,0)) %>%
  run()

get_mon_arrivals(sim)
plot_resource_usage(sim, "t-rex")
