library(simmer)

triage <- trajectory("Patient's path") %>%
  log_("arrive at queue") %>%
  trap("check mailbox") %>%
  wait() %>%
  log_("triage")

mailbox <- trajectory() %>%
  timeout(function() rgamma(1, shape = 2, rate = 0.1)) %>%
  send("check mailbox") %>%
  rollback(2, Inf)

set.seed(1234)

env <- simmer()  %>%
  add_generator("person", triage, function() rpois(1, 2)) %>%
  add_generator("mailbox", mailbox, at(0)) %>%
  run(25)

triage <- trajectory("Patient's path") %>%
  log_("arrive at queue") %>%
  batch(Inf, timeout=function() rgamma(1, shape = 2, rate = 0.1)) %>%
  separate() %>%
  log_("triage")

set.seed(1234)

env <- simmer()  %>%
  add_generator("person", triage, function() rpois(1, 2)) %>%
  run(25)

################################################################################

t <- trajectory(verbose=T) %>%
  batch(3, timeout=1, permanent=F, rule=NULL) %>%
  seize("dummy", 1) %>%
  timeout(1) %>%
  release("dummy", 1) %>%
  separate() %>%
  timeout(1)

env <- simmer(verbose=TRUE) %>%
  add_resource("dummy", 1, 0) %>%
  add_generator("arrival", t, at(0, 1, 2)) %>%
  run()

get_mon_arrivals(env, per_resource=TRUE)
get_mon_arrivals(env, per_resource=FALSE)
