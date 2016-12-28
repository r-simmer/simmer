library(simmer)
library(simmer.plot)

t0 <- trajectory() %>%
  set_attribute("health", function() sample(40:80,1)) %>%
  set_attribute("nurses_to_seize",
                function(attrs){
                  if(attrs[["health"]]<50) 2
                  else 1
                }) %>%
  seize("nurse",
        function(attrs){attrs[["nurses_to_seize"]]}) %>%
  timeout(function(attrs){(100 - attrs[["health"]])}) %>%
  set_attribute("health",
                function(attrs){
                  min(attrs[["health"]] + sample(attrs[["health"]]:100, 1), 100)}) %>%
  release("nurse",
          function(attrs){attrs[["nurses_to_seize"]]}) %>%

  ## some other functionality
  ## simply print the attrs using a 0 timeout
  timeout(function(attrs){print(attrs); 0})

env<-simmer() %>%
  add_generator("test", t0, at(seq(0,1000,200)), mon=2) %>%
  add_resource("nurse", 2) %>%
  run()

attributes <- env %>% get_mon_attributes()

plot(env, "res", "usage", "nurse", items="server", steps=T)
plot(env, "attr")
plot(env, "attr", keys="health")
