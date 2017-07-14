library(simmer)
library(simmer.plot)

t0 <- trajectory() %>%
  set_attribute("health", function() sample(40:80,1)) %>%
  set_attribute("nurses_to_seize",
                function(){
                  if(get_attribute(env, "health")<50) 2
                  else 1
                }) %>%
  seize("nurse",
        function(){get_attribute(env, "nurses_to_seize")}) %>%
  timeout(function(){(100 - get_attribute(env, "health"))}) %>%
  set_attribute("health",
                function(){
                  min(get_attribute(env, "health") + sample(get_attribute(env, "health"):100, 1), 100)}) %>%
  release("nurse",
          function(){get_attribute(env, "nurses_to_seize")})

env<-simmer() %>%
  add_generator("test", t0, at(seq(0,1000,200)), mon=2) %>%
  add_resource("nurse", 2)
env %>% run()

env %>% get_mon_attributes()

plot(env, "res", "usage", "nurse", items="server", steps=T)
plot(env, "attr")
plot(env, "attr", keys="health")
