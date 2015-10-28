library(simmer)

arrival <- Simmer.make_closure(is_gen=F, {
  function(env, mu) {
    yield(env$request("server", 1))
    t <- rexp(1, mu)
    yield(env$timeout(t))
  }
})

exp_generator <- Simmer.make_closure(is_gen=T, {
  function(env, la, mu) {
    while (T) {
      t <- rexp(1, la)
      yield(env$timeout(t))
      a <- arrival(env, mu)
      env$process(a)
    }
  }
})

env <- Simmer.Env$new() $
  resource("server", capacity=1, mon=T) $
  process(exp_generator(env, 1, 2))

env$run(until=1000)
